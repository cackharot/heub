{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Services.AuthenticationService(
    validateUser
  , createUser
  , findUser
  , searchUsers
  , convertUserToDocument
  , convertUserDocumentToUser
  )
where

import Database.MongoDB    (MongoContext(..), Database, Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find, findOne,
                            host, insert, insertMany, master, project, rest,
                            select, sort, (=:))
import Data.Maybe
import Control.Monad
import Control.Monad.Trans (lift, liftIO)
import qualified Data.Bson as B
import Data.Time.Clock
import Data.Time.Calendar

import App.Model

validateUser :: IO MongoContext -> TUsername -> TPassword -> IO Bool
validateUser dbContext username password = do
  maybeUser <- findUser dbContext username
  return $ maybe False (\x->B.at "password" x == password) maybeUser

createUser :: IO MongoContext -> User -> IO Value
createUser dbContext user = connectDb dbContext $ insert "users" (convertUserToDocument user)

findUser :: IO MongoContext -> TUsername -> IO (Maybe Document)
findUser dbContext username = connectDb dbContext $ findOne $ select ["username" =: username] "users"

searchUsers :: IO MongoContext -> IO [User]
searchUsers dbContext = do
  users <- connectDb dbContext $ rest =<< find (select [] "users")
  return $ map convertUserDocumentToUser users

convertUserDocumentToUser :: Document -> User
convertUserDocumentToUser doc = User (show (B.at "_id" doc :: B.ObjectId)) (B.at "username" doc) (B.at "password" doc)
  (B.at "display_name" doc) (B.at "email" doc) (B.at "dob" doc)
  (B.at "created_at" doc) (B.at "created_by" doc) (B.at "updated_at" doc) (B.at "updated_by" doc) (B.at "status" doc)

convertUserToDocument :: User -> Document
convertUserToDocument user = [
  "username" =: username user, "password" =: password user, "email" =: email user,
  "display_name" =: displayName user, "dob" =: dateOfBirth user,
  "created_by" =: createdBy user, "created_at" =: createdAt user,
  "updated_by" =: updatedBy user, "updated_at" =: updatedAt user,
  "status" =: status user]

connectDb :: IO MongoContext -> Action IO a -> IO a
connectDb dbContext cmd = do
  ctx <- dbContext
  e <- access (mongoPipe ctx) (mongoAccessMode ctx) (mongoDatabase ctx) cmd
  close (mongoPipe ctx)
  return e
