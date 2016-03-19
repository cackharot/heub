{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.UserService
where

import           Snap.Snaplet
import           Snap.Core
import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans (liftIO)

import App.Model
import Services.AuthenticationService

data UserService = UserService

makeLenses ''UserService

userApiRoutes :: [(B.ByteString, Handler b UserService ())]
userApiRoutes = [("search", method GET fetchAllUsers)]

fetchAllUsers :: Handler b UserService ()
fetchAllUsers = do
  _ <- liftIO $ createUser buildUser
  users <- liftIO $ searchUsers
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeLBS . encode $ users

buildUser = User 1 "admin" "pass@123" "display admin" "admin@app.com" 19880909 20160313 "admin" 20160316 "admin" True

userServiceApiInit :: SnapletInit b UserService
userServiceApiInit = makeSnaplet "userService" "User service api" Nothing $ do
                      addRoutes userApiRoutes
                      return UserService
