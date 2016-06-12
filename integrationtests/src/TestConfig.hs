{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, ScopedTypeVariables #-}
--{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE DeriveGeneric #-}

module TestConfig (
    getConfigFilename
  , getEndpointUrl
  , infoEndpoint
  ) where

import Control.Lens
import Data.Maybe
import Data.List
import Data.Aeson
import Data.Aeson.Types (defaultOptions)

import qualified Data.ByteString.Char8 as B

import System.IO
import System.Environment

import Data.Text (Text, append)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Text.RawString.QQ
import GHC.Generics (Generic)

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

--defaultConfig = [r|
--baseUrl: http://localhost:8000
--endpoints:
--  - name: info
--    url: /api/info
-- |]

infoEndpoint :: String -> String
infoEndpoint = getEndpointUrl "info"

getEndpointUrl :: Text -> String -> String
getEndpointUrl key configContents = let config = Y.decodeEither (B.pack configContents) :: Either String AppConfig in
  case config of
    Left e -> error e
    Right c -> T.unpack value
      where
        ep = fromJust $ find (\x-> endpointName x == key) (endpoints c)
        value = baseUrl c `append` endpointUrl ep

getConfigFilename :: IO FilePath
getConfigFilename = do
  mf <- lookupEnv "HEUB_TEST_CONFIG"
  return $ fromMaybe "config/local.yml" mf
