{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Services.AuthenticationService
where

import Database.MongoDB    (Database, Action, Document, Document, Value, access,
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

validateUser :: TUsername -> TPassword -> IO Bool
validateUser username password = do
  maybeUser <- findUser username
  return $ maybe False (\x->B.at "password" x == password) maybeUser

createUser :: User -> IO Value
createUser user = connectDb $ insert "users" (convertUserToDocument user)

findUser :: TUsername -> IO (Maybe Document)
findUser username = connectDb $ findOne $ select ["username" =: username] "users"

searchUsers :: IO [User]
searchUsers = do
  users <- connectDb $ rest =<< find (select [] "users")
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

dbName :: Database
dbName = "testAppDb"

connectDb cmd = do
  pipe <- connect $ host "localhost"
  e <- access pipe master dbName cmd
  close pipe
  return e
