{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

import           Api.Core
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I

import           Application


-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = []

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    api <- nestSnaplet "api" api apiInit
    addRoutes routes
    return $ App api
