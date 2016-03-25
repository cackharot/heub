{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}

module SmokeTestSpec (spec) where

import Test.Hspec
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as LB

{-# ANN module "HLint: ignore Redundant do" #-}

infoEndpoint = "http://localhost:8000/api/info"

spec :: Spec
spec =
  describe "Application basic smoke test" $ do
    it "should expose /info endpoint" $ do
      r <- get infoEndpoint
      let
        sc  = (r ^. responseStatus . statusCode)
        serviceName = (r ^? responseBody . key "name")
        in
        do
          sc `shouldBe` 200
          serviceName `shouldBe` Just (String "hueb service")
