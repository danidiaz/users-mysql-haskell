{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Data.ByteString.Char8 as Char8

import Control.Exception
import System.Environment

import Database.MySQL.Base
import Web.Users.Types
import Web.Users.MySQL

import Test.Tasty
import Test.Tasty.HUnit

-- https://github.com/agrafix/users/blob/master/users-test/src/Web/Users/TestSpec.hs

test_mysql_host,test_mysql_port,test_mysql_database,test_mysql_user,test_mysql_password :: String
test_mysql_host = "TEST_MYSQL_HOST"
test_mysql_port = "TEST_MYSQL_PORT"
test_mysql_database = "TEST_MYSQL_DATABASE"
test_mysql_user = "TEST_MYSQL_USER"
test_mysql_password = "TEST_MYSQL_PASSWORD"

withConn :: (Backend -> IO a) -> IO a
withConn f = do
    connInfo <- ConnectInfo <$>                 getEnv test_mysql_host
                            <*> (read       <$> getEnv test_mysql_port)
                            <*> (Char8.pack <$> getEnv test_mysql_database)
                            <*> (Char8.pack <$> getEnv test_mysql_user)
                            <*> (Char8.pack <$> getEnv test_mysql_password)
    getEnv test_mysql_host >>= print
    getEnv test_mysql_port >>= print
    getEnv test_mysql_database >>= print
    getEnv test_mysql_user >>= print
    getEnv test_mysql_password >>= print
    bracket (connect connInfo) 
            close 
            (f . backend)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    [
        createAndDelete
    ]

createAndDelete :: TestTree
createAndDelete = testCase "createAndDelete" $ withConn $ \backend' -> do
    initUserBackend backend' 
    count <- countUsers backend'
    destroyUserBackend backend'
    assertEqual "user count" count 0

