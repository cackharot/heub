{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}

module RegressionTestSpec (spec) where

import GHC.Exts
import Test.Hspec
import Network.Wreq
import Control.Lens
import Data.Maybe
import Data.List
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Aeson.Lens (key, nth)
import Control.Monad.Trans (liftIO)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import TestConfig

{-# ANN module "HLint: ignore Redundant do" #-}

testUsername = "reg_user_test_1"
testUserPassword = "nisd&134NK@!"

spec :: Spec
spec =
  describe "Application regression tests" $ do
    it "should create test user if not exists" $ do
      configContents <- getConfigFilename >>= readFile
      r <- get $ getEndpointUrl "search_user" configContents
      let
        serviceName = (r ^? responseBody . key "name")
        in
        do
          liftIO $ print r
          serviceName `shouldBe` Just (String "hueb service")
