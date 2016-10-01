{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Web.Users.MySQL () where

import Data.Int(Int64)

import Web.Users.Types
import Database.MySQL.Base

newtype Conn a = Conn { getConn :: a } deriving (Eq,Show)

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
