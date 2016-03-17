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

import App.Model

validateUser :: TUsername -> TPassword -> IO Bool
validateUser username password = do
  maybeUser <- findUser username
  return $ checkMaybeBool $ fmap (checkPassword password) maybeUser

checkMaybeBool :: Maybe Bool -> Bool
checkMaybeBool Nothing = False
checkMaybeBool (Just a) = a == True

checkPassword :: TPassword -> Document -> Bool
checkPassword password user = (B.at "password" user) == password

createUser :: User -> IO Value
createUser user = connectDb $ insert "users" (convertUserToDocument user)

findUser :: TUsername -> IO (Maybe Document)
findUser username = connectDb $ findOne $ select ["username" =: username] "users"

convertUserToDocument user = ["username" =: username user, "password" =: password user, "email" =: email user]

dbName :: Database
dbName = "testAppDb"

connectDb cmd = do
  pipe <- connect $ host "localhost"
  e <- access pipe master dbName cmd
  close pipe
  return e
