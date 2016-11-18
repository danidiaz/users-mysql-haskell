{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NumDecimals #-}

module Main where

import Data.Monoid
import Data.Foldable
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import Control.Concurrent
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
doConnect :: IO MySQLConn
doConnect = do
    connInfo <- ConnectInfo <$>                 getEnv test_mysql_host
                            <*> (read       <$> getEnv test_mysql_port)
                            <*> (Char8.pack <$> getEnv test_mysql_database)
                            <*> (Char8.pack <$> getEnv test_mysql_user)
                            <*> (Char8.pack <$> getEnv test_mysql_password)
    connect connInfo

makeTest :: IO MySQLConn -> TestName -> (Backend -> Assertion) -> TestTree
makeTest connMaker tname f = withResource (do b <- backend <$> connMaker 
                                              initUserBackend b
                                              return b) 
                                          destroyUserBackend
                                          (\action -> testCase tname $ action >>= f)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = withResource doConnect
                     close
                     (\connio -> let mk = makeTest connio
                                 in  testGroup "Tests" 
                                     [ mk "testCreateAndDelete" testCreateAndDelete
                                     , mk "testCountUsers0" testCountUsers0
                                     , mk "testCreateUser" testCreateUser
                                     , mk "testGetUserIdByName" testGetUserIdByName
                                     , mk "testGetUserById" testGetUserById
                                     , mk "testListUsers" testListUsers
                                     , mk "testUpdateUser" testUpdateUser
                                     , mk "testDeleteUser" testDeleteUser
                                     , mk "testActivation" testActivation
                                     , mk "testPasswordReset" testPasswordReset
                                     , mk "testAuth" testAuth
                                     , mk "testHousekeep" testHousekeep
                                     ])

testCreateAndDelete:: Backend -> Assertion
testCreateAndDelete _ = return ()

testCountUsers0 :: Backend -> Assertion
testCountUsers0 b = do
    count <- countUsers b
    assertEqual "user count" count 0

testCreateUser :: Backend -> Assertion
testCreateUser b = do
    let user = User "name" "name@mail.com" (PasswordHash "pass") True
    Right _ <- createUser b user
    count <- countUsers b
    assertEqual "user count" count 1
    Right _ <- createUser b (user { u_name = "name2" , u_email = "name2@mail.com"})
    count2 <- countUsers b
    assertEqual "user count" count2 2
    Left UsernameAndEmailAlreadyTaken <- createUser b user
    Left EmailAlreadyTaken <- createUser b (user { u_name = "name3" })
    Left UsernameAlreadyTaken <- createUser b (user { u_email = "name3@mail.com" })
    let usernopass = User "name4" "name4@mail.com" PasswordHidden True
    Left InvalidPassword <- createUser b usernopass
    count3 <- countUsers b
    assertEqual "user count" count3 2
    return ()

testGetUserIdByName :: Backend -> Assertion
testGetUserIdByName b = do
    let user = User "userbyname" "userbyname@mail.com" (PasswordHash "pass") True
        user2 = User "userbyname2" "userbyname2@mail.com" (PasswordHash "pass") True
    Right usrid <- createUser b user
    Right usrid2 <- createUser b user2
    Just usrid' <- getUserIdByName b "userbyname"
    assertEqual "ids coincide" usrid usrid'
    Just usrid'' <- getUserIdByName b "userbyname@mail.com"
    assertEqual "ids coincide" usrid usrid''
    Just usrid2' <- getUserIdByName b "userbyname2"
    assertEqual "ids coincide" usrid2 usrid2'
    Just usrid2'' <- getUserIdByName b "userbyname2@mail.com"
    assertEqual "ids coincide" usrid2 usrid2''
    Nothing <- getUserIdByName b "userbynamexxxxxx"
    return ()

createTenUsers :: Backend -> IO [User]
createTenUsers b = do 
    let suffixes = map (Text.pack . show) [(1::Integer)..10]
        users = map (\i -> User ("name"<>i) ("name"<>i<>"@mail.com") (PasswordHash "pass") False)
                    suffixes
    for_ users $ \u -> do
        Right _ <- createUser b u
        pure ()
    return users

testGetUserById :: Backend -> Assertion
testGetUserById b = do
    users <- createTenUsers b
    Just u <- getUserById b 3
    assertEqual "returned user" (hidePassword (users !! 2)) u
    Nothing <- getUserById b 9999
    pure ()

testListUsers :: Backend -> Assertion
testListUsers b = do
    users <- createTenUsers b
    count <- countUsers b
    assertEqual "user count" count 10
    rus1 <- listUsers b Nothing (SortAsc UserFieldId) 
    assertEqual "roundtrip" (hidePassword <$> users) (snd <$> rus1)
    rus2 <- listUsers b Nothing (SortDesc UserFieldId) 
    assertEqual "reversed roundtrip" (hidePassword <$> users) (snd <$> reverse rus2)
    rus3 <- listUsers b (Just (2,4)) (SortAsc UserFieldId) 
    assertEqual "with offsets" (hidePassword <$> (take 4 . drop 2 $ users)) (snd <$> rus3)
    pure ()

testUpdateUser :: Backend -> Assertion
testUpdateUser b = do
    _ <- createTenUsers b
    Just usrid <- getUserIdByName b "name3"
    Left UsernameAlreadyExists <- updateUser b usrid (\u -> u { u_name = "name4" })
    Left EmailAlreadyExists <- updateUser b usrid (\u -> u { u_email = "name4@mail.com" })
    Just u1 <- getUserById b usrid
    Right () <- updateUser b usrid (\u -> u { u_name = "namex", u_email = "namex@mail.com" })
    Just u2 <- getUserById b usrid
    assertEqual "updated" (hidePassword $ u1 { u_name = "namex", u_email = "namex@mail.com" }) u2
    Right () <- updateUser b usrid (\u -> u { u_password = PasswordHash "zzz" })
    pure ()

testDeleteUser :: Backend -> Assertion
testDeleteUser b = do
    _ <- createTenUsers b
    count1 <- countUsers b
    Just usrid <- getUserIdByName b "name3"
    _ <- deleteUser b usrid
    Nothing <- getUserById b usrid
    count2 <- countUsers b
    assertEqual "deleted" (pred count1) count2

testActivation :: Backend -> Assertion
testActivation b = do
    _ <- createTenUsers b
    do Just usrid <- getUserIdByName b "name3"
       ActivationToken tok <- requestActivationToken b usrid 3600
       assertEqual "token length" 36 (Text.length tok)
       Right () <- activateUser b (ActivationToken tok)
       Just u <- getUserById b usrid
       assertBool "is active" (u_active u)
    do Just usrid <- getUserIdByName b "name4"
       ActivationToken tok <- requestActivationToken b usrid 3600
       assertEqual "token length" 36 (Text.length tok)
       Left TokenInvalid <- activateUser b (ActivationToken "bogus activation token")
       return ()
    do Just usrid <- getUserIdByName b "name5"
       ActivationToken tok <- requestActivationToken b usrid 3600
       assertEqual "token length" 36 (Text.length tok)
       Right ()          <- activateUser b (ActivationToken tok)
       Left TokenInvalid <- activateUser b (ActivationToken tok)
       return ()

testPasswordReset :: Backend -> Assertion
testPasswordReset b = do
    _ <- createTenUsers b
    do let name = "name3"
       Just usrid <- getUserIdByName b name
       tok <- requestPasswordReset b usrid 10
       Just usr <- verifyPasswordResetToken b tok
       assertEqual "verification user" name (u_name usr) 
       return ()
    do let name = "name4"
       Just usrid <- getUserIdByName b name
       tok <- requestPasswordReset b usrid 1
       threadDelay 3e6
       Nothing <- verifyPasswordResetToken b tok
       return ()
    do let name = "name5"
       Just usrid <- getUserIdByName b name
       tok <- requestPasswordReset b usrid 10
       Right () <- applyNewPassword b tok (PasswordHash "newpasswd")
       Nothing <- verifyPasswordResetToken b tok
       return ()

testAuth :: Backend -> Assertion
testAuth b = do
    _ <- createTenUsers b
    let name = "name3"
        passwd = PasswordPlain "foooo"
    Just usrid <- getUserIdByName b name
    tok <- requestPasswordReset b usrid 10
    Right () <- applyNewPassword b tok (makePassword passwd)
    do Just sessId <- authUser b name passwd 10
       Nothing <- verifySession b (SessionId "bloxoxs") 10 
       Just _ <- verifySession b sessId 10 
       return ()
    do Just sessId <- authUser b name passwd 10
       destroySession b sessId
       Nothing <- verifySession b sessId 10 
       return ()
    do Just sessId <- authUser b name passwd 1
       threadDelay 3e6
       Nothing <- verifySession b sessId 10 
       return ()
    do Just sessId <- authUser b name passwd 3
       threadDelay 2e6
       Just _ <- verifySession b sessId 10 
       threadDelay 4e6
       Just _ <- verifySession b sessId 10 
       return ()
    return ()

testHousekeep :: Backend -> Assertion
testHousekeep b = do
    _ <- createTenUsers b
    housekeepBackend b
    return ()
