{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.UserService
where

import           Snap.Snaplet
import           Snap.Core
import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Data.Bson
import Control.Lens
import Control.Monad.State.Class
import Control.Monad.Trans (liftIO)
import Control.Applicative
import Data.Maybe

import App.Model
import Services.AuthenticationService

import EncUtil (encryptMessage)

import Database.MongoDB    (MongoContext(..), Database, Action, Document, Document, Value, access,
                            close, connect, master, host)

data UserService = UserService {
  _dbContext :: IO MongoContext
}

makeLenses ''UserService

userApiRoutes :: [(B.ByteString, Handler b UserService ())]
userApiRoutes = [("login",  method POST createAuthToken)
                ,("search", method GET fetchAllUsers)
                ,("create", method PUT createUserDetails)]


createAuthToken :: Handler b UserService ()
createAuthToken = do
  dbCtx <- gets _dbContext
  muser <- getPostParam "username"
  mpass <- getPostParam "password"
  modifyResponse $ setResponseCode 200
  isValid <- liftIO $ validateUser dbCtx (getDefaultString muser) (getDefaultString mpass)
  if isValid then
    do
      userDoc <- liftIO $ findUser dbCtx (getDefaultString muser)
      writeLBS . encode $ createToken $ convertUserDocumentToUser $ fromJust userDoc
  else
    writeLBS . encode $ AuthTokenResponse 2001 "Invalid username or password given!" []

createToken :: User -> String
createToken u = B.unpack $ encryptMessage $ B.pack (_id u ++ ":" ++ username u)

getDefaultString :: Maybe B.ByteString -> String
getDefaultString a = B.unpack $ fromMaybe "" a

createUserDetails :: Handler b UserService ()
createUserDetails = do
  dbCtx <- gets _dbContext
  rq <- getRequest
  userJsonString <- readRequestBody 4000
  case eitherDecode userJsonString of
    Left e -> invalidRequest $ B.pack e
    Right user ->
      case validateUserDetails user of
        e@(x:xs)-> throwError e
        [] -> do
                userId <- liftIO $ createUser dbCtx user
                modifyResponse $ setResponseStatus 202 "Created"
                writeLBS . encode $ show userId

uValidations :: [ValidationModel User]
uValidations = [
    ValidationModel (\x -> length (username x) > 4) "username" "should be atleast 4 characters long"
  , ValidationModel (validEmail . email) "email" "enter a valid email address"
  , ValidationModel (validPass . password) "password" "should be 6-20 characters long with atleast one !@#$%^&*"
  ]

validEmail a = length a > 5 && '@' `elem` a
validPass p = length p > 6 && length p < 20 && any (\x -> x `elem` ['!', '#', '$', '%', '^', '&', '@', '*']) p

validateUserDetails :: User -> [ValidationError]
validateUserDetails user = mapMaybe (applyValidation user) uValidations

applyValidation :: a -> ValidationModel a -> Maybe ValidationError
applyValidation a (ValidationModel f fieldName message) = if f a then
    Nothing
  else
    Just $ ValidationError fieldName message

throwError :: ToJSON a => a -> Handler b UserService ()
throwError e = do
  modifyResponse $ setResponseStatus 412 "Validation error"
  writeLBS . encode $ e

invalidRequest :: B.ByteString -> Handler b a ()
invalidRequest message = do
  modifyResponse $ setResponseStatus 402 "Bad request"
  writeBS message

fetchAllUsers :: Handler b UserService ()
fetchAllUsers = do
  dbCtx <- gets _dbContext
  users <- liftIO $ searchUsers dbCtx
  modifyResponse $ setHeader "Content-Type" "application/json"
  modifyResponse $ setResponseCode 200
  writeLBS . encode $ users


userServiceApiInit :: SnapletInit b UserService
userServiceApiInit = makeSnaplet "userService" "User service api" Nothing $ do
                      addRoutes userApiRoutes
                      return $ UserService dbConfiguration

dbName :: Database
dbName = "devAppDb"

dbConfiguration :: IO MongoContext
dbConfiguration = do
  pipe <- connect $ host "localhost"
  return $ MongoContext pipe master dbName
