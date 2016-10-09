{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Users.MySQL (Backend,backend) where

import Data.Int(Int64)

import Web.Users.Types
import Database.MySQL.Base

newtype Backend = Backend { getConn :: MySQLConn }

backend :: MySQLConn -> Backend
backend = Backend

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

instance UserStorageBackend Backend where

    type UserId Backend = Int64

    initUserBackend (Backend conn) =
        undefined
    destroyUserBackend (Backend conn) =
        undefined
    housekeepBackend (Backend conn) =
        undefined
    -- | Retrieve a user id from the database
    getUserIdByName (Backend conn) username =
        undefined
    listUsers (Backend conn) mLimit sortField =
        undefined
    countUsers (Backend conn) =
        undefined
    createUser (Backend conn) user =
        undefined
    updateUser (Backend conn) userId updateFun =
        undefined
    deleteUser (Backend conn) userId =
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
