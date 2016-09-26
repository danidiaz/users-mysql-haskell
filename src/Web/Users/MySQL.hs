{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Web.Users.MySQL () where

import Data.Int(Int64)

import Web.Users.Types
import Database.MySQL.Base

instance UserStorageBackend MySQLConn where
    type UserId MySQLConn = Int64

    initUserBackend conn =
        undefined
    destroyUserBackend conn =
        undefined
    housekeepBackend conn =
        undefined
    -- | Retrieve a user id from the database
    getUserIdByName conn username =
        undefined
    listUsers conn mLimit sortField =
        undefined
    countUsers conn =
        undefined
    createUser conn user =
        undefined
    updateUser conn userId updateFun =
        undefined
    deleteUser conn userId =
        undefined
    createSession conn userId sessionTtl =
        undefined
    withAuthUser conn username authFn action =
        undefined
    verifySession conn (SessionId sessionId) extendTime =
        undefined
    destroySession conn (SessionId sessionId) = 
        undefined
    requestPasswordReset conn userId timeToLive =
        undefined
    requestActivationToken conn userId timeToLive =
        undefined
    activateUser conn (ActivationToken token) =
        undefined
    verifyPasswordResetToken conn (PasswordResetToken token) =
        undefined
    applyNewPassword conn (PasswordResetToken token) password =
        undefined
