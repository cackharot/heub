{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}
module ModelSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import App.Model
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as LB

spec :: Spec
spec =
  describe "Check user data" $ do
    it "should create user data" $ do
      buildUser `shouldBe` buildUser
    it "returns username from the user data type" $ do
      username buildUser `shouldBe` "admin"
    it "should encode user to json string" $ do
      encode buildUser `shouldBe` LB.fromString "{\"email\":\"admin@app.com\",\"username\":\"admin\",\"id\":1}"
    it "should encode user array to json string" $ do
      encode [buildUser] `shouldBe` LB.fromString "[{\"email\":\"admin@app.com\",\"username\":\"admin\",\"id\":1}]"

buildUser = User 1 "admin" "pass@123" "display admin" "admin@app.com" 19880909 20160313 "admin" 20160316 "admin" True
