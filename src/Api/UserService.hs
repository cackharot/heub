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
import Data.Maybe

import App.Model
import Services.AuthenticationService

data UserService = UserService

makeLenses ''UserService

userApiRoutes :: [(B.ByteString, Handler b UserService ())]
userApiRoutes = [("login", method POST validateUserCredentials)
                ,("search", method GET fetchAllUsers)
                ,("create", method PUT createUserDetails)]


validateUserCredentials :: Handler b UserService ()
validateUserCredentials = do
  muser <- getPostParam "username"
  mpass <- getPostParam "password"
  modifyResponse $ setResponseCode 200
  let
    username = getDefaultString muser
    password = getDefaultString mpass in
    do
      isValid <- liftIO $ validateUser username password
      if isValid then
        writeLBS . encode $ username ++ ":" ++ password
      else
        writeLBS . encode $ "Invalid username or password! Given (" ++ username ++ ":" ++ password ++ ")"

getDefaultString :: Maybe B.ByteString -> String
getDefaultString Nothing = ""
getDefaultString (Just a)  = B.unpack a

createUserDetails :: Handler b UserService ()
createUserDetails = do
  rq <- getRequest
  userJsonString <- readRequestBody 4000
  case eitherDecode userJsonString of
    Left e -> invalidRequest $ B.pack e
    Right user ->
      do
        liftIO $ createUser user
        modifyResponse $ setResponseStatus 202 "Accepted"
        writeBS "Accepted"

invalidRequest :: B.ByteString -> Handler b a ()
invalidRequest message = do
  modifyResponse $ setResponseStatus 402 "Bad request"
  writeBS message

fetchAllUsers :: Handler b UserService ()
fetchAllUsers = do
  --_ <- liftIO $ createUser buildUser
  users <- liftIO searchUsers
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeLBS . encode $ users

buildUser = User 1 "admin" "pass@123" "display admin" "admin@app.com" 19880909 20160313 "admin" 20160316 "admin" True

userServiceApiInit :: SnapletInit b UserService
userServiceApiInit = makeSnaplet "userService" "User service api" Nothing $ do
                      addRoutes userApiRoutes
                      return UserService
