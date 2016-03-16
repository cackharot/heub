{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Services.AuthenticationService
where

import Database.MongoDB    (Database, Action, Document, Document, Value, access,
                            close, connect, delete, exclude, find, findOne,
                            host, insert, insertMany, master, project, rest,
                            select, sort, (=:))
import Control.Monad.Trans (liftIO)

import App.Model

validateUser :: String -> String -> Bool
validateUser "" _ = False
validateUser _ "" = False
validateUser "" "" = False
validateUser username password = username == "admin" && password == "pass"

createUser :: User -> IO Value
createUser user = connectDb $ insert "users" (convertUserToDocument user)

findUser username = connectDb $ findOne $ select ["username" =: username] "users"

convertUserToDocument user = ["username" =: username user, "password" =: password user, "email" =: email user]

dbName :: Database
dbName = "testAppDb"

connectDb cmd = do
  pipe <- connect $ host "localhost"
  e <- access pipe master dbName cmd
  close pipe
  return e
