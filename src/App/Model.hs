{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Model
where

import Data.Aeson
import Data.Maybe
import Control.Monad

type TUsername = String
type TPassword = String

type DateTime = Integer

data User = User {
  id :: Integer
, username :: TUsername
, password :: TPassword
, displayName :: String
, email :: String
, dateOfBirth :: DateTime
, createdAt :: DateTime
, createdBy :: String
, updatedAt :: DateTime
, updatedBy :: String
, status :: Bool
} deriving (Show, Eq, Read)


instance ToJSON User where
  toJSON (User id username password displayName email dateOfBirth createdAt createdBy updatedBy updatedAt status) =
    object [ "id" .= id, "username" .= username, "email" .= email, "display_name" .= displayName, "date_of_birth" .= dateOfBirth
      , "created_at" .= createdAt, "created_by" .= createdBy, "updated_at" .= updatedAt, "updated_by" .= updatedBy
      , "status" .= status ]

instance FromJSON User where
  parseJSON (Object v) = User <$>
                          v .:? "id" .!= 1 <*>
                          v .: "username" <*>
                          v .: "password" <*>
                          v .: "display_name" <*>
                          v .: "email" <*>
                          v .:? "date_of_birth" .!= 19880909 <*>
                          v .:? "created_at" .!= 20160313 <*>
                          v .:? "created_by" .!= "admin" <*>
                          v .:? "updated_at" .!= 20160316 <*>
                          v .:? "updated_by" .!= "admin" <*>
                          v .:? "status" .!= True
  parseJSON _          = mzero
