{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}

module ModelSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import App.Model
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as LB

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec =
  describe "Check user data" $ do
    it "should create user data" $ do
      buildUser `shouldBe` buildUser
    it "returns username from the user data type" $ do
      username buildUser `shouldBe` "admin"
    it "should encode user to json string" $ do
      encode buildUser `shouldBe` LB.fromString "{\"email\":\"admin@app.com\",\"status\":true,\"display_name\":\"display admin\",\"date_of_birth\":19880909,\"username\":\"admin\",\"created_by\":\"admin\",\"updated_at\":\"admin\",\"created_at\":20160313,\"id\":1,\"updated_by\":20160316}"
    it "should encode user array to json string" $ do
      encode [buildUser] `shouldBe` LB.fromString "[{\"email\":\"admin@app.com\",\"status\":true,\"display_name\":\"display admin\",\"date_of_birth\":19880909,\"username\":\"admin\",\"created_by\":\"admin\",\"updated_at\":\"admin\",\"created_at\":20160313,\"id\":1,\"updated_by\":20160316}]"
    it "should decode user json to user object" $ do
      (decode userJsonString :: Maybe User) `shouldBe` Just buildUser

userJsonString = "{\"email\":\"admin@app.com\",\"username\":\"admin\",\"password\":\"pass@123\",\"display_name\":\"display admin\"}"

buildUser = User 1 "admin" "pass@123" "display admin" "admin@app.com" 19880909 20160313 "admin" 20160316 "admin" True
