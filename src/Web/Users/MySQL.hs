{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Web.Users.MySQL (Backend,backend) where

import Data.Maybe
import Data.String
import Data.List
import qualified Data.Text as Text
import Data.Int(Int64)
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import System.IO.Streams
import System.IO.Streams.List
import Web.Users.Types
import Database.MySQL.Base

newtype Backend = Backend MySQLConn

backend :: MySQLConn -> Backend
backend = Backend

-- https://github.com/agrafix/users/blob/master/users-postgresql-simple/src/Web/Users/Postgresql.hs
-- http://hackage.haskell.org/package/mysql-haskell-0.5.0.0/docs/Database-MySQL-Base.html
-- http://dev.mysql.com/doc/refman/5.7/en/create-table.html

-- NOTE: The implementation has lots of incomplete patterns.

usersTableSQL :: Query
usersTableSQL = 
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
userTokenTableSQL :: Query
userTokenTableSQL =
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

extendTokenSQL :: Query
extendTokenSQL = 
     "UPDATE login_token\
     \ SET valid_until =\
     \ (CASE WHEN (NOW() + INTERVAL ? second) > valid_until THEN (NOW() + INTERVAL ? second) ELSE valid_until END)\
     \ WHERE token_type = ?\
     \ AND token = ?;"

createUserNameIndexSQL :: Query
createUserNameIndexSQL = "create index l_username on login (username);"

createEmailIndexSQL :: Query
createEmailIndexSQL = "create index l_email on login (email);"

createTokenIndexSQL :: Query
createTokenIndexSQL = "create index lt_token on login_token (token);"

userByNameSQL :: Query
userByNameSQL = 
	"SELECT lid, username, password, email, is_active \
	\ FROM login WHERE (username = ? OR email = ?) LIMIT 1;"

tokenIdSQL :: Query
tokenIdSQL = 
	"SELECT lid FROM login_token WHERE token_type = ? \
	\ AND token = ? AND valid_until > NOW() LIMIT 1;"

insertTokenSQL :: Query
insertTokenSQL = 
    "INSERT INTO login_token (token, token_type, lid, valid_until)\
    \ VALUES (?, ?, ?, timestampadd(SECOND,?,NOW()));"

instance UserStorageBackend Backend where

    type UserId Backend = Int64

    initUserBackend (Backend conn) = do
        _ <- execute_ conn usersTableSQL
        _ <- execute_ conn createUserNameIndexSQL
        _ <- execute_ conn createEmailIndexSQL
        _ <- execute_ conn userTokenTableSQL
        _ <- execute_ conn createTokenIndexSQL
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
    updateUser b userId updateFun =
        do mUser <- getUserById b userId
           case mUser of
             Nothing ->
                 return $ Left UserDoesntExist
             Just origUser -> 
                 runExceptT $ do let newUser = updateFun origUser
                                 doesUsernameAlreadyExist b newUser origUser
                                 doesEmailAlreadyExist b newUser origUser
                                 liftIO $ performUserUpdate b newUser userId
    deleteUser (Backend conn) userId =
        do _ <- execute conn "DELETE FROM login WHERE lid = ?;" [MySQLInt64 userId]
           return ()
    authUser b username password sessionTtl =
        withAuthUser b username (\user -> verifyPassword password $ u_password user) $ \userId ->
           SessionId <$> createToken b "session" userId sessionTtl
    createSession b userId sessionTtl =
        do mUser <- getUserById b userId
           case (mUser :: Maybe User) of
             Nothing -> return Nothing
             Just _ -> Just . SessionId <$> createToken b "session" userId sessionTtl
    withAuthUser (Backend conn) username authFn action =
        do resultSet <- drain $ query conn 
								 	  userByNameSQL
									  [MySQLText username
									  ,MySQLText username
									  ]
           case resultSet of
             [ MySQLInt64 userId, MySQLText name, MySQLText password, MySQLText email, MySQLInt8 is_active ] : _ 
               -> do let user = convertUserTuple (name, PasswordHash password, email, is_active /= 0)
                     if authFn user
                        then Just <$> action userId
                        else return Nothing
             _ -> return Nothing
    verifySession b (SessionId sessionId) extendTime =
        do mUser <- getTokenOwner b "session" sessionId
           case mUser of
             Nothing -> return Nothing
             Just userId ->
                 do extendToken b "session" sessionId extendTime
                    return (Just userId)
    destroySession b (SessionId sessionId) = deleteToken b "session" sessionId
    requestActivationToken b userId timeToLive =
        do token <- createToken b "activation" userId timeToLive
           return $ ActivationToken token
    activateUser b (ActivationToken token) =
        do mUser <- getTokenOwner b "activation" token
           case mUser of
             Nothing ->
                 return $ Left TokenInvalid
             Just userId ->
                 do _ <-
                        updateUser b userId $ \user -> user { u_active = True }
                    deleteToken b "activation" token
                    return $ Right ()
    requestPasswordReset b userId timeToLive =
        do token <- createToken b "password_reset" userId timeToLive
           return $ PasswordResetToken token
    verifyPasswordResetToken b (PasswordResetToken token) =
        do mUser <- getTokenOwner b "password_reset" token
           case mUser of
             Nothing -> return Nothing
             Just userId -> getUserById b userId
    applyNewPassword b (PasswordResetToken token) password =
        do mUser <- getTokenOwner b "password_reset" token
           case mUser of
             Nothing ->
                 return $ Left TokenInvalid
             Just userId ->
                 do _ <-
                        updateUser b userId $ \user -> user { u_password = password }
                    deleteToken b "password_reset" token
                    return $ Right ()

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
        [MySQLInt64 emailCount] : _ <- drain $ query conn 
                                                     "SELECT COUNT(lid) FROM login WHERE lower(email) = lower(?) LIMIT 1;" 
                                                     [MySQLText $ u_email user]
        [MySQLInt64 loginCount] : _ <- drain $ query conn 
                                                     "SELECT COUNT(lid) FROM login WHERE username = ? LIMIT 1;" 
                                                     [MySQLText $ u_name user]
        case (emailCount == 1,loginCount == 1) of
            (True, True)   -> return $ Left UsernameAndEmailAlreadyTaken
            (True, False)  -> return $ Left EmailAlreadyTaken
            (False, True)  -> return $ Left UsernameAlreadyTaken
            -- http://stackoverflow.com/questions/17112852/get-the-new-record-primary-key-id-from-mysql-insert-query
            -- http://dev.mysql.com/doc/refman/5.7/en/getting-unique-id.html
            (False, False) -> do
                _ <- execute conn 
                             "INSERT INTO login (username, password, email, is_active) VALUES (?, ?, ?, ?)"
                             [ MySQLText $ u_name user
                             , MySQLText $ password
                             , MySQLText $ u_email user
                             , MySQLInt8 $ if u_active user then 1 else 0
                             ]
                [MySQLInt64U u_id] : _ <- drain $ query_ conn "SELECT LAST_INSERT_ID()"
                return $ Right (fromIntegral u_id)

doesUsernameAlreadyExist :: Backend -> User -> User -> ExceptT UpdateUserError IO ()
doesUsernameAlreadyExist (Backend conn) newUser origUser = do
    when (u_name newUser /= u_name origUser) $ do
        [MySQLInt64 counter] : _ <- liftIO $ drain $ query conn 
														   "SELECT COUNT(lid) FROM login where username = ?;" 
                                                           [MySQLText $ u_name newUser]
        when (counter /= 0) $ do
            throwE UsernameAlreadyExists

doesEmailAlreadyExist :: Backend -> User -> User -> ExceptT UpdateUserError IO ()
doesEmailAlreadyExist (Backend conn) newUser origUser = do
    when (u_email newUser /= u_email origUser) $ do
        [MySQLInt64 counter] : _ <- liftIO $ drain $ query conn 
														   "SELECT COUNT(lid) FROM login where lower(email) = lower(?);" 
                                                           [MySQLText $ u_email newUser]
        when (counter /= 0) $ do
            throwE EmailAlreadyExists

performUserUpdate :: Backend -> User -> Int64 -> IO ()
performUserUpdate (Backend conn) newUser userId = do
    _ <- execute conn 
                 "UPDATE login SET username = ?, email = ?, is_active = ? WHERE lid = ?;" 
                 [ MySQLText $ u_name newUser
                 , MySQLText $ u_email newUser
                 , MySQLInt8 $ if u_active newUser then 1 else 0
                 , MySQLInt64 userId
                 ]
    case u_password newUser of
          PasswordHash p ->
              do _ <- execute conn 
                              "UPDATE login SET password = ? WHERE lid = ?;" 
                              [ MySQLText p
                              , MySQLInt64 userId
                              ]
                 return ()
          _ -> return ()

drain :: IO ([ColumnDef], InputStream [MySQLValue]) -> IO [[MySQLValue]]
drain action = do
    (_,ist) <- action
    System.IO.Streams.List.toList ist

createToken :: Backend -> String -> Int64 -> NominalDiffTime -> IO Text.Text
createToken (Backend conn) tokenType userId timeToLive = do
    tok <- Text.pack . UUID.toString <$> UUID.nextRandom
    _ <- execute conn 
				 insertTokenSQL
                 [MySQLText   $ tok 
                 ,MySQLText   $ Text.pack tokenType 
                 ,MySQLInt64  $ userId 
                 ,MySQLInt64  $ fromIntegral (convertTtl timeToLive)
                 ]
    return tok

getTokenOwner :: Backend -> String -> Text.Text -> IO (Maybe Int64)
getTokenOwner (Backend conn) tokenType token =
    case UUID.fromString (Text.unpack token) of
      Nothing -> return Nothing
      Just _  ->
          do resultSet <- drain $ query conn 
										tokenIdSQL
                                        [MySQLText $ Text.pack tokenType
                                        ,MySQLText $ token
                                        ]
             case resultSet of
                 [MySQLInt64 userId] : _ -> return (Just userId)
                 []                      -> return Nothing

deleteToken :: Backend -> String -> Text.Text -> IO ()
deleteToken (Backend conn) tokenType token =
    case UUID.fromString (Text.unpack token) of
      Nothing   -> return ()
      Just _ ->
          do _ <- execute conn "DELETE FROM login_token WHERE token_type = ? AND token = ?;" 
                               [MySQLText $ Text.pack tokenType
                               ,MySQLText token
                               ]
             return ()

extendToken :: Backend -> String -> Text.Text -> NominalDiffTime -> IO ()
extendToken (Backend conn) tokenType token timeToLive =
    case UUID.fromString (Text.unpack token) of
      Nothing -> return ()
      Just _  ->
          do _ <-
                  execute conn extendTokenSQL 
                               [MySQLInt64  $ fromIntegral (convertTtl timeToLive) 
                               ,MySQLInt64  $ fromIntegral (convertTtl timeToLive) 
                               ,MySQLText   $ Text.pack tokenType 
                               ,MySQLText   $ token
                               ]
             return ()

convertTtl :: NominalDiffTime -> Int
convertTtl = round
