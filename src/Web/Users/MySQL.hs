{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Users.MySQL () where

import Data.Int(Int64)

import Web.Users.Types
import Database.MySQL.Base

newtype Conn a = Conn { getConn :: a } deriving (Eq,Show)

-- http://dev.mysql.com/doc/refman/5.7/en/create-table.html

createUsersTable :: Query
createUsersTable = 
    "CREATE TABLE IF NOT EXISTS login (\
        \ lid             BIGINT NOT NULL AUTO_INCREMENT,\
        \ created_at      TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \ username        VARCHAR(64)    NOT NULL UNIQUE,\
        \ password        VARCHAR(255)   NOT NULL,\
        \ email           VARCHAR(64)   NOT NULL UNIQUE,\
        \ is_active       BOOLEAN NOT NULL DEFAULT FALSE,\
        \ CONSTRAINT \"l_pk\" PRIMARY KEY (lid)\
        \ );"

-- http://mysqlserverteam.com/storing-uuid-values-in-mysql-tables/
createUserTokenTable :: Query
createUserTokenTable =
    "CREATE TABLE IF NOT EXISTS login_token (\
        \ltid             BIGINT NOT NULL AUTO_INCREMENT,\
        \token            UUID UNIQUE,\
        \token_type       VARCHAR(64) NOT NULL,\
        \lid              INTEGER NOT NULL,\
        \created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \valid_until      TIMESTAMP NOT NULL,\
        \CONSTRAINT \"lt_pk\" PRIMARY KEY (ltid),\
        \CONSTRAINT \"lt_lid_fk\" FOREIGN KEY (lid) REFERENCES login ON DELETE CASCADE\
        \);"

instance UserStorageBackend (Conn MySQLConn) where

    type UserId (Conn MySQLConn) = Int64

    initUserBackend (Conn conn) =
        undefined
    destroyUserBackend (Conn conn) =
        undefined
    housekeepBackend (Conn conn) =
        undefined
    -- | Retrieve a user id from the database
    getUserIdByName (Conn conn) username =
        undefined
    listUsers (Conn conn) mLimit sortField =
        undefined
    countUsers (Conn conn) =
        undefined
    createUser (Conn conn) user =
        undefined
    updateUser (Conn conn) userId updateFun =
        undefined
    deleteUser (Conn conn) userId =
        undefined
    createSession (Conn conn) userId sessionTtl =
        undefined
    withAuthUser (Conn conn) username authFn action =
        undefined
    verifySession (Conn conn) (SessionId sessionId) extendTime =
        undefined
    destroySession (Conn conn) (SessionId sessionId) = 
        undefined
    requestPasswordReset (Conn conn) userId timeToLive =
        undefined
    requestActivationToken (Conn conn) userId timeToLive =
        undefined
    activateUser (Conn conn) (ActivationToken token) =
        undefined
    verifyPasswordResetToken (Conn conn) (PasswordResetToken token) =
        undefined
    applyNewPassword (Conn conn) (PasswordResetToken token) password =
        undefined
