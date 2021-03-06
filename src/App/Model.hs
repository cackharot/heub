{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric     #-}

module App.Model (
      TUsername
    , TPassword
    , User(..)
    , Role(..)
    , UserRole(..)
    , Privilege(..)
    , RolePrivilege(..)
    , ValidationError(..)
    , ValidationModel(..)
    , AuthTokenResponse(..)
  )
where

import Data.Aeson
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Data.DateTime (DateTime)
import Control.Monad
import GHC.Generics (Generic)

type TUsername = String
type TPassword = String

data User = User {
  _id :: String
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

data AuthTokenResponse = AuthTokenResponse {
    errorCode :: Integer
  , errorMessage :: String
  , details :: [String]
} deriving (Show, Generic)

instance ToJSON AuthTokenResponse

data Role = Role {
    roleId :: String
  , roleName :: String
  , roleDescription :: String
  , roleStatus :: Bool
} deriving (Show, Eq, Read, Generic)

instance FromJSON Role
instance ToJSON Role

data UserRole = UserRole {
    userRoleId :: String
  , userRoleUserId :: String
  , userRoleRoleId :: String
  , userRoleStatus :: Bool
} deriving (Show, Eq, Read, Generic)

instance FromJSON UserRole
instance ToJSON UserRole

data Privilege = Privilege {
    privilegeId :: String
  , privilegeName :: String
  , privilegeDescription :: String
  , privilegeStatus :: Bool
} deriving (Show, Eq, Read, Generic)

instance FromJSON Privilege
instance ToJSON Privilege

data RolePrivilege = RolePrivilege {
    rolePrivilegeId :: String
  , rolePrivilegeRoleId :: String
  , rolePrivilegePrivilegeId :: String
  , rolePrivilegeStatus :: Bool
} deriving (Show, Eq, Read, Generic)

instance FromJSON RolePrivilege
instance ToJSON RolePrivilege

data ValidationError = ValidationError {
    fieldName :: String
  , message :: String
} deriving (Show, Eq, Generic)

instance FromJSON ValidationError
instance ToJSON ValidationError

data ValidationModel a = ValidationModel (a -> Bool) String String

instance ToJSON User where
  toJSON (User _id username password displayName email dateOfBirth createdAt createdBy updatedBy updatedAt status) =
    object [ "id" .= _id, "username" .= username, "email" .= email, "display_name" .= displayName, "date_of_birth" .= dateOfBirth
      , "created_at" .= createdAt, "created_by" .= createdBy, "updated_at" .= updatedAt, "updated_by" .= updatedBy
      , "status" .= status ]

instance FromJSON User where
  parseJSON (Object v) = User <$>
                          v .:? "_id" .!= "1" <*>
                          v .: "username" <*>
                          v .: "password" <*>
                          v .: "display_name" <*>
                          v .: "email" <*>
                          v .:? "date_of_birth" .!=  sampleTestTime <*>
                          v .:? "created_at" .!= sampleTestTime <*>
                          v .:? "created_by" .!= "admin" <*>
                          v .:? "updated_at" .!= sampleTestTime <*>
                          v .:? "updated_by" .!= "admin" <*>
                          v .:? "status" .!= True
  parseJSON _          = mzero

sampleTestTime = UTCTime (fromGregorian 2016 03 11) (secondsToDiffTime 0)
