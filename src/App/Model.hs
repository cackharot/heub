module App.Model
where

type TPassword = String

type DateTime = Integer

data User = User {
  id :: Integer
, username :: String
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
