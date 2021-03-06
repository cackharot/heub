{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}

module SmokeTestSpec (spec) where

import Test.Hspec
import Network.Wreq
import Control.Lens
import Data.Maybe
import Data.List
import Data.Aeson
import Data.Aeson.Lens (key, nth)

import TestConfig

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec =
  describe "Application basic smoke test" $ do
    it "should expose /info endpoint" $ do
      configContents <- getConfigFilename >>= readFile
      r <- get $ infoEndpoint configContents
      let
        sc  = (r ^. responseStatus . statusCode)
        serviceName = (r ^? responseBody . key "name")
        in
        do
          sc `shouldBe` 200
          serviceName `shouldBe` Just (String "hueb service")
