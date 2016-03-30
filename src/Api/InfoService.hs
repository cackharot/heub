{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.InfoService (InfoService, infoServiceApiInit)
where

import           Snap.Snaplet
import           Snap.Core
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import Data.Aeson
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans (liftIO)

import Lib

data InfoService = InfoService

makeLenses ''InfoService

infoApiRoutes :: [(B.ByteString, Handler b InfoService ())]
infoApiRoutes = [("info", method GET getInfo)
                ,("env", withAuth $ method GET getEnv)
                ,("config", method GET getConfig)
                ,("getremoteip", method GET getRemoteIP)]

getInfo :: Handler b InfoService ()
getInfo = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeText "{\"name\":\"hueb service\",\"description\":\"Provides base template to start new web services in haskell\",\"status\": \"UP\"}"

getEnv :: Handler b InfoService ()
getEnv = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeText "env protected endpoint"

getConfig :: Handler b InfoService ()
getConfig = do
  r <- getRequest
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeLBS . encode $ map (\(x,y) -> (B.unpack $ CI.original x, B.unpack y)) $ listHeaders r

getRemoteIP :: Handler b InfoService ()
getRemoteIP = do
  r <- getRequest
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeBS $ rqRemoteAddr r

infoServiceApiInit :: SnapletInit b InfoService
infoServiceApiInit = makeSnaplet "infoService" "Provies info and health endpoints to reports healthiness of the service" Nothing $ do
                      addRoutes infoApiRoutes
                      return InfoService
