{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.InfoService (InfoService, infoServiceApiInit)
where

import           Snap.Snaplet
import           Snap.Core
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as D
import Data.Aeson
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans (liftIO)

data InfoService = InfoService

makeLenses ''InfoService

withAuth :: Handler a b () -> Handler a b ()
withAuth h = do
  rq <- getRequest
  let mh = getHeader "Authorization" rq
  let ph = parseAuthorizationHeader mh
  --liftIO $ print ph
  isValid <- liftIO $ testAuth ph
  if not isValid then
    writeText "Access Denied!"
  else
    h

parseAuthorizationHeader :: Maybe B.ByteString -> Maybe (B.ByteString, B.ByteString)
parseAuthorizationHeader Nothing = Nothing
parseAuthorizationHeader (Just x) = case (B.split ' ' x) of
  ("Basic" : y : _) ->
    if B.length y == 0 then
      Nothing
    else
      let decodedValue=D.decode y in
        case decodedValue of
          Left e -> Nothing
          Right val ->
            case (B.split ':' val) of
              (user:pass:_) -> Just (user, pass)
              _ -> Nothing
  _ -> Nothing

testAuth :: Maybe (B.ByteString, B.ByteString) -> IO Bool
testAuth Nothing = return False
testAuth (Just (user,pass)) = return isValidUser
  where
   isValidUser = user == "test" && pass == "pass@123"

infoApiRoutes :: [(B.ByteString, Handler b InfoService ())]
infoApiRoutes = [("info", method GET getInfo)
                ,("env", withAuth $ method GET getEnv)]

getInfo :: Handler b InfoService ()
getInfo = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeText ("{\"name\":\"hueb service\",\"description\":\"Provides base template to start new web services in haskell\",\"status\": \"UP\"}")

getEnv :: Handler b InfoService ()
getEnv = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeText "env protected endpoint"

infoServiceApiInit :: SnapletInit b InfoService
infoServiceApiInit = makeSnaplet "infoService" "Provies info and health endpoints to reports healthiness of the service" Nothing $ do
                      addRoutes infoApiRoutes
                      return InfoService
