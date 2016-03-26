{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}

module AuthenticationServiceSpec (spec) where

import Test.Hspec
import Test.QuickCheck
--import System.Locale
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format

import App.Model
import Services.AuthenticationService

import Database.MongoDB    (Action, Document, Document, Value, Database, dropDatabase, access,
                            close, connect, delete, exclude, find,
                            host, insert, insertMany, master, project, rest,
                            select, sort, (=:))

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

spec :: Spec
spec = around withCleanDatabase $
  describe "Authentication Service tests" $ do
    it "returns True given valid username and password" $ do
      _ <- buildUser >>= createUser
      (validateUser "admin" "pass@123") >>= (`shouldBe` True)

    it "returns False given valid username and invalid password" $ do
      _ <- buildUser >>= createUser
      (validateUser "admin" "invalid_pass") >>= (`shouldBe` False)

    it "returns False given invalid username" $ do
      (validateUser "invalid" "invalid") >>= (`shouldBe` False)

    it "creates new user in database given valid user details" $ do
        user <- buildUser
        _id <- createUser user
        users <- db $ rest =<< find (select [] "users")
        length users `shouldBe` 1
        head users `shouldBe` ("_id" =: _id):convertUserToDocument user

    it "should find user by username" $ do
        user <- buildUser
        _id <- createUser user
        actualUser <- findUser "admin"
        actualUser `shouldBe` Just (("_id" =: _id):convertUserToDocument user)

    it "should search all users" $ do
      _ <- buildUser >>= createUser
      users <- searchUsers
      length users `shouldBe` 1
      username (head users) `shouldBe` "admin"


buildUser :: IO User
buildUser = do
  ct <- getCurrentTime
  let
    tstr = formatTime defaultTimeLocale "%c" ct
    t = parseTimeOrError True defaultTimeLocale "%c" tstr in
    return $ User "" "admin" "pass@123" "display admin" "admin@app.com" t t "admin" t "admin" True

testDBName :: Database
testDBName = "testAppDb"

db :: Action IO a -> IO a
db action = do
    pipe <- connect (host "localhost")
    result <- access pipe master testDBName action
    close pipe
    return result

withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = dropDB >> action () >> dropDB >> return ()
  where
    dropDB = db $ dropDatabase testDBName
