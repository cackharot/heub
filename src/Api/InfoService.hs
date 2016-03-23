{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.InfoService (InfoService, infoServiceApiInit)
where

import           Snap.Snaplet
import           Snap.Core
import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans (liftIO)

import Lib

data InfoService = InfoService

makeLenses ''InfoService

infoApiRoutes :: [(B.ByteString, Handler b InfoService ())]
infoApiRoutes = [("info", method GET getInfo)
                ,("env", withAuth $ method GET getEnv)]

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

infoServiceApiInit :: SnapletInit b InfoService
infoServiceApiInit = makeSnaplet "infoService" "Provies info and health endpoints to reports healthiness of the service" Nothing $ do
                      addRoutes infoApiRoutes
                      return InfoService
