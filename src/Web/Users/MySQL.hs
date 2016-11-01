{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Web.Users.MySQL (Backend,backend) where

import Data.Maybe
import Data.String
import Data.List
import qualified Data.Text as Text
import Data.Int(Int64)
import Web.Users.Types
import Database.MySQL.Base
import System.IO.Streams
import System.IO.Streams.List

newtype Backend = Backend { getConn :: MySQLConn }

backend :: MySQLConn -> Backend
backend = Backend

-- https://github.com/agrafix/users/blob/master/users-postgresql-simple/src/Web/Users/Postgresql.hs
-- http://hackage.haskell.org/package/mysql-haskell-0.5.0.0/docs/Database-MySQL-Base.html
-- http://dev.mysql.com/doc/refman/5.7/en/create-table.html

-- NOTE: The implementation has lots of incomplete patterns.

createUsersTable :: Query
createUsersTable = 
    "CREATE TABLE IF NOT EXISTS login (\
        \ lid             BIGINT NOT NULL AUTO_INCREMENT,\
        \ created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \ username        VARCHAR(64)    NOT NULL UNIQUE,\
        \ password        VARCHAR(255)   NOT NULL,\
        \ email           VARCHAR(64)   NOT NULL UNIQUE,\
        \ is_active       BOOLEAN NOT NULL DEFAULT FALSE,\
        \ CONSTRAINT l_pk PRIMARY KEY (lid)\
        \ );"

-- http://mysqlserverteam.com/storing-uuid-values-in-mysql-tables/
-- http://dev.mysql.com/doc/refman/5.7/en/timestamp-initialization.html
createUserTokenTable :: Query
createUserTokenTable =
    "CREATE TABLE IF NOT EXISTS login_token (\
        \ltid             BIGINT NOT NULL AUTO_INCREMENT,\
        \token            VARCHAR(36) UNIQUE,\
        \token_type       VARCHAR(64) NOT NULL,\
        \lid              BIGINT NOT NULL,\
        \created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \valid_until      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \CONSTRAINT lt_pk PRIMARY KEY (ltid),\
        \CONSTRAINT lt_lid_fk FOREIGN KEY (lid) REFERENCES login(lid) ON DELETE CASCADE\
        \);"

instance UserStorageBackend Backend where

    type UserId Backend = Int64

    initUserBackend (Backend conn) = do
        _ <- execute_ conn createUsersTable
        _ <- execute_ conn createUserTokenTable
        return ()
    destroyUserBackend (Backend conn) = do
        _ <- execute_ conn "drop table login_token;"
        _ <- execute_ conn "drop table login;"
        return ()
    housekeepBackend (Backend conn) = do
        _ <- execute_ conn "DELETE FROM login_token WHERE valid_until < NOW();"
        return ()
    getUserIdByName (Backend conn) username = do
        rs <- drain $ query conn "SELECT lid FROM login WHERE (username = ? OR email = ?) LIMIT 1;" 
                                 [MySQLText username,MySQLText username]
        case listToMaybe rs of
            Nothing                 -> return Nothing
            Just (MySQLInt64 uid:_) -> return (Just uid)
    getUserById = getUserById'
    listUsers = listUsers'
    countUsers (Backend conn) = do
        [MySQLInt64 count] : _ <- drain $ query_ conn "SELECT COUNT(lid) FROM login;"
        return count
    createUser b user =
        case u_password user of
            PasswordHash p -> createUser' b user p
            _              -> return $ Left InvalidPassword
    updateUser (Backend conn) userId updateFun =
        undefined
    deleteUser (Backend conn) userId =
        undefined
    authUser conn username password sessionTtl =
        undefined
    createSession (Backend conn) userId sessionTtl =
        undefined
    withAuthUser (Backend conn) username authFn action =
        undefined
    verifySession (Backend conn) (SessionId sessionId) extendTime =
        undefined
    destroySession (Backend conn) (SessionId sessionId) = 
        undefined
    requestPasswordReset (Backend conn) userId timeToLive =
        undefined
    requestActivationToken (Backend conn) userId timeToLive =
        undefined
    activateUser (Backend conn) (ActivationToken token) =
        undefined
    verifyPasswordResetToken (Backend conn) (PasswordResetToken token) =
        undefined
    applyNewPassword (Backend conn) (PasswordResetToken token) password =
        undefined

convertUserTuple :: (Text.Text, Password, Text.Text, Bool) -> User
convertUserTuple (username, password, email, isActive) =
    User username email password isActive

getUserById' :: Backend -> Int64 -> IO (Maybe User)
getUserById' (Backend conn) usrid = do
    rs <- drain $ query conn stmt [MySQLInt64 usrid] 
    case listToMaybe rs of
        Nothing -> 
            return Nothing
        Just [MySQLText username, MySQLText email, MySQLInt8 isActive] -> 
            return $ Just (convertUserTuple (username, PasswordHidden, email, isActive /= 0))
    where
    stmt = "SELECT username, email, is_active FROM login WHERE lid = ? LIMIT 1;"

listUsers' :: Backend -> Maybe (Int64, Int64) -> SortBy UserField -> IO [(UserId Backend, User)]
listUsers' (Backend conn) mLimit sortField = do
    -- NOTE: the mysql-haskell Query type does not have a Monoid instance.
    -- NOTE: booleans are mapped as TINYINT.
    rs <- drain $ query_ conn (fromString (intercalate " " [baseQuery,getOrderBy sortField,limitPart mLimit]))
    return $ Prelude.map convertUser rs
    where
    baseQuery = "SELECT lid, username, email, is_active FROM login"
    limitPart = \case
        Nothing             -> ""
        Just (start, count) -> "LIMIT " ++ show count ++ " OFFSET " ++ show start 
    convertUser [MySQLInt64 lid, MySQLText username, MySQLText email, MySQLInt8 isActive] =
        (lid, convertUserTuple (username, PasswordHidden, email, isActive /= 0))

getOrderBy :: SortBy UserField -> String
getOrderBy sb =
    "ORDER BY " ++
    case sb of
      SortAsc t -> getSqlField t  ++ " ASC"
      SortDesc t -> getSqlField t ++ " DESC"

getSqlField :: UserField -> String
getSqlField userField =
    case userField of
      UserFieldId -> "lid"
      UserFieldActive -> "is_active"
      UserFieldEmail -> "email"
      UserFieldName -> "username"
      UserFieldPassword -> "password"

createUser' :: Backend -> User -> Text.Text -> IO (Either CreateUserError Int64)
createUser' (Backend conn) user password = do
        [MySQLInt64 emailCount] : _ <- drain $ query conn "SELECT COUNT(lid) FROM login WHERE lower(email) = lower(?) LIMIT 1;" 
                                                          [MySQLText $ u_email user]
        [MySQLInt64 loginCount] : _ <- drain $ query conn "SELECT COUNT(lid) FROM login WHERE username = ? LIMIT 1;" 
                                                          [MySQLText $ u_name user]
        case (emailCount == 1,loginCount == 1) of
            (True, True)   -> return $ Left UsernameAndEmailAlreadyTaken
            (True, False)  -> return $ Left EmailAlreadyTaken
            (False, True)  -> return $ Left UsernameAlreadyTaken
            -- http://stackoverflow.com/questions/17112852/get-the-new-record-primary-key-id-from-mysql-insert-query
            -- http://dev.mysql.com/doc/refman/5.7/en/getting-unique-id.html
            (False, False) -> do
                _ <- execute conn "INSERT INTO login (username, password, email, is_active) VALUES (?, ?, ?, ?)"
                                  [ MySQLText $ u_name user
                                  , MySQLText $ password
                                  , MySQLText $ u_email user
                                  , MySQLInt8 $ if u_active user then 1 else 0
                                  ]
                [MySQLInt64U u_id] : _ <- drain $ query_ conn "SELECT LAST_INSERT_ID()"
                return $ Right (fromIntegral u_id)

drain :: IO ([ColumnDef], InputStream [MySQLValue]) -> IO [[MySQLValue]]
drain action = do
    (_,ist) <- action
    System.IO.Streams.List.toList ist
