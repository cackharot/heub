{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}

module ModelSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import App.Model
import Data.Aeson
import Data.Time.Clock
import Data.Time.Calendar

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
      encode buildUser `shouldBe` LB.fromString ujs
    it "should encode user array to json string" $ do
      encode [buildUser] `shouldBe` LB.fromString ("["++ ujs ++"]")
    it "should decode user json to user object" $ do
      (decode userJsonString :: Maybe User) `shouldBe` Just buildUser

userJsonString = "{\"email\":\"admin@app.com\",\"username\":\"admin\",\"password\":\"pass@123\",\"display_name\":\"display admin\"}"

ujs = "{\"email\":\"admin@app.com\",\"status\":true,\"display_name\":\"display admin\",\"date_of_birth\":\"2016-03-11T00:00:00.000000000000Z\",\"username\":\"admin\",\"created_by\":\"admin\",\"updated_at\":\"admin\",\"created_at\":\"2016-03-11T00:00:00.000000000000Z\",\"id\":\"1\",\"updated_by\":\"2016-03-11T00:00:00.000000000000Z\"}"

buildUser = User "1" "admin" "pass@123" "display admin" "admin@app.com" sampleTestTime sampleTestTime "admin" sampleTestTime "admin" True

sampleTestTime = UTCTime (fromGregorian 2016 03 11) (secondsToDiffTime 0)
