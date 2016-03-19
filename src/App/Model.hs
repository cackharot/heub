{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module App.Model
where

import           Data.Aeson

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
  toJSON (User id username password displayName email dateOfBirth createdAt createdBy updatedBy updatedAt status) = object [ "id" .= id, "username" .= username, "email" .= email ]
