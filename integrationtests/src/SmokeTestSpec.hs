{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric #-}

module SmokeTestSpec (spec) where

import Test.Hspec
import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Types (defaultOptions)
import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as LB

import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Text.RawString.QQ
import GHC.Generics (Generic)

{-# ANN module "HLint: ignore Redundant do" #-}

data EndPointDetails = EndPointDetails {
    endpointName :: Text
  , endpointUrl :: Text
} deriving (Eq, Show, Generic)

data AppConfig =
  AppConfig {
    baseUrl :: Text
  , endpoints  :: [EndPointDetails]
  } deriving (Eq, Show, Generic)

instance FromJSON EndPointDetails where
  parseJSON (Object v) = EndPointDetails <$>
                         v .: "name" <*>
                         v .: "url"
  parseJSON _ = error "Failed to parse EndPointDetails json"

instance FromJSON AppConfig where
  parseJSON = genericParseJSON defaultOptions

defaultConfig = [r|
baseUrl: http://localhost:8000
endpoints:
  - name: info
    url: /api/info
|]

infoEndpoint :: String
infoEndpoint = let config = Y.decodeEither defaultConfig :: Either String AppConfig in
  case config of
    Left e -> error e
    Right c -> let
                infoEP = head (endpoints c)
                infoUrl = baseUrl c `append` (endpointUrl infoEP)
                in
                  T.unpack infoUrl

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
