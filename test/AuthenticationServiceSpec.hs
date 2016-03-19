{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}
module AuthenticationServiceSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import App.Model
import Services.AuthenticationService

import Database.MongoDB    (Action, Document, Document, Value, Database, dropDatabase, access,
                            close, connect, delete, exclude, find,
                            host, insert, insertMany, master, project, rest,
                            select, sort, (=:))

spec :: Spec
spec = around withCleanDatabase $ do
  describe "Authentication Service tests" $ do
    it "returns True given valid username and password" $ do
      _ <- createUser buildUser
      (validateUser "admin" "pass@123") >>= (`shouldBe` True)

    it "returns False given valid username and invalid password" $ do
      _ <- createUser buildUser
      (validateUser "admin" "invalid_pass") >>= (`shouldBe` False)

    it "returns False given invalid username" $ do
      validateUser "invalid" "invalid" >>= (`shouldBe` False)

    it "creates new user in database given valid user details" $ do
        _id <- createUser buildUser
        actual <- db $ rest =<< find (select [] "users")
        actual `shouldBe` [["_id" =: _id, "username" =: username buildUser, "password" =: password buildUser, "email" =: email buildUser]]

    it "returns monogodb document given User data type" $ do
      let user = buildUser
          userDocument = ["username" =: username user, "password" =: password user, "email" =: email user] in
        convertUserToDocument user `shouldBe` userDocument

    it "should find user by username" $ do
        _id <- createUser buildUser
        user <- findUser "admin"
        user `shouldBe` Just ["_id" =: _id, "username" =: username buildUser, "password" =: password buildUser, "email" =: email buildUser]

    it "should search all users" $ do
      createUser buildUser
      users <- searchUsers
      length users `shouldBe` 1
      users !! 0 `shouldBe` buildUser


buildUser = User 1 "admin" "pass@123" "display admin" "admin@app.com" 19880909 20160313 "admin" 20160316 "admin" True

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
