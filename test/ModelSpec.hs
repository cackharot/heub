{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}
module ModelSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import App.Model

spec :: Spec
spec =
  describe "Check user data" $ do
    it "should create user data" $ do
      buildUser `shouldBe` buildUser
    it "returns username from the user data type" $ do
      username buildUser `shouldBe` "admin"

buildUser = User 1 "admin" "pass@123" "display admin" "admin@app.com" 19880909 20160313 "admin" 20160316 "admin" True
