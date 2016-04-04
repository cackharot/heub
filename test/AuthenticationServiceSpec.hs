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

import Database.MongoDB    (MongoContext(..), Action, Document, Document, Value, Database, dropDatabase, access,
                            close, connect, delete, exclude, find,
                            host, insert, insertMany, master, project, rest,
                            select, sort, (=:))

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Redundant bracket" #-}

spec :: Spec
spec = around withCleanDatabase $
  describe "Authentication Service tests" $ do
    it "returns True given valid username and password" $ do
      _ <- buildUser >>= createUser dbConfiguration
      (validateUser dbConfiguration "admin" "pass@123") >>= (`shouldBe` True)

    it "returns False given valid username and invalid password" $ do
      _ <- buildUser >>= createUser dbConfiguration
      (validateUser dbConfiguration "admin" "invalid_pass") >>= (`shouldBe` False)

    it "returns False given invalid username" $ do
      (validateUser dbConfiguration "invalid" "invalid") >>= (`shouldBe` False)

    it "creates new user in database given valid user details" $ do
        user <- buildUser
        _id <- createUser dbConfiguration user
        users <- db dbConfiguration $ rest =<< find (select [] "users")
        length users `shouldBe` 1
        head users `shouldBe` ("_id" =: _id):convertUserToDocument user

    it "should find user by username" $ do
        user <- buildUser
        _id <- createUser dbConfiguration user
        actualUser <- findUser dbConfiguration "admin"
        actualUser `shouldBe` Just (("_id" =: _id):convertUserToDocument user)

    it "should search all users" $ do
      _ <- buildUser >>= createUser dbConfiguration
      users <- searchUsers dbConfiguration
      length users `shouldBe` 1
      username (head users) `shouldBe` "admin"


buildUser :: IO User
buildUser = do
  ct <- getCurrentTime
  let
    tstr = formatTime defaultTimeLocale "%c" ct
    t = parseTimeOrError True defaultTimeLocale "%c" tstr in
    return $ User "" "admin" "pass@123" "display admin" "admin@app.com" t t "admin" t "admin" True

dbName :: Database
dbName = "testAppDb"

dbConfiguration :: IO MongoContext
dbConfiguration = do
  pipe <- connect $ host "localhost"
  return $ MongoContext pipe master dbName

db :: IO MongoContext -> Action IO a -> IO a
db dbContext cmd = do
  ctx <- dbContext
  e <- access (mongoPipe ctx) (mongoAccessMode ctx) (mongoDatabase ctx) cmd
  close (mongoPipe ctx)
  return e

withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = dropDB >> action () >> dropDB >> return ()
  where
    dropDB = db dbConfiguration $ dropDatabase dbName
