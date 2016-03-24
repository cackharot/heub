{-# LANGUAGE OverloadedStrings #-}

module Lib (
  withAuth
  )
where

import Snap.Snaplet
import Snap.Core
import qualified Data.ByteString.Base64 as D
import qualified Data.ByteString.Char8 as B

import Control.Monad.Trans (liftIO)

import Services.AuthenticationService

{- |
    An Snap transparent handler to handle HTTP Basic authentication
-}
withAuth :: Handler a b () -> Handler a b ()
withAuth nextHandler = do
  rq <- getRequest
  let mh = getHeader "Authorization" rq
  let ph = parseAuthorizationHeader mh
  --liftIO $ print ph
  if ph == Nothing then
    do
      throwChallenge
  else
    do
      isValid <- liftIO $ testAuth ph
      if isValid then
        nextHandler
      else
        throwAccessDenied

parseAuthorizationHeader :: Maybe B.ByteString -> Maybe (B.ByteString, B.ByteString)
parseAuthorizationHeader Nothing = Nothing
parseAuthorizationHeader (Just x) = case B.split ' ' x of
  ("Basic" : y : _) ->
    if B.length y == 0 then
      Nothing
    else
      let decodedValue=D.decode y in
        case decodedValue of
          Left e -> Nothing
          Right val ->
            case B.split ':' val of
              (user:pass:_) -> Just (user, pass)
              _ -> Nothing
  _ -> Nothing

testAuth :: Maybe (B.ByteString, B.ByteString) -> IO Bool
testAuth Nothing = return False
testAuth (Just (user,pass)) = validateUser (B.unpack user) (B.unpack pass)

throwChallenge :: Handler a b ()
throwChallenge = do
  modifyResponse $ setResponseCode 401
  modifyResponse $ setHeader "WWW-Authenticate" "Basic releam=heub"
  writeBS ""

throwAccessDenied :: Handler a b ()
throwAccessDenied = do
  modifyResponse $ setResponseCode 403
  writeText "Access Denied!"
