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

-- https://dev.mysql.com/doc/refman/5.5/en/implicit-commit.html
withConn :: (Backend -> IO a) -> IO a
withConn f = do
    connInfo <- ConnectInfo <$>                 getEnv test_mysql_host
                            <*> (read       <$> getEnv test_mysql_port)
                            <*> (Char8.pack <$> getEnv test_mysql_database)
                            <*> (Char8.pack <$> getEnv test_mysql_user)
                            <*> (Char8.pack <$> getEnv test_mysql_password)
    bracket (connect connInfo) 
            close 
            (f . backend)

withDb :: (Backend -> IO a) -> IO a
withDb f = withConn $ \b -> do initUserBackend b
                               r <- f b
                               destroyUserBackend b
                               return r

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "Tests" 
    [
        testCreateAndDelete
    ,   testCountUsers
    ]

testCreateAndDelete:: TestTree
testCreateAndDelete = testCase "createAndDelete" $ withDb $ \_ -> do return ()

testCountUsers :: TestTree
testCountUsers = testCase "countUsers" $ withDb $ \b -> do
    count <- countUsers b
    assertEqual "user count" count 0
