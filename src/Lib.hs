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

{- |
    An Snap transparent handler to handle HTTP Basic authentication
-}
withAuth :: Handler a b () -> Handler a b ()
withAuth h = do
  rq <- getRequest
  let mh = getHeader "Authorization" rq
  let ph = parseAuthorizationHeader mh
  --liftIO $ print ph
  isValid <- liftIO $ testAuth ph
  if not isValid then
    do
      modifyResponse $ setResponseCode 401
      modifyResponse $ setHeader "Authorization" "Basic releam=heub"
      writeText ""
  else
    h

parseAuthorizationHeader :: Maybe B.ByteString -> Maybe (B.ByteString, B.ByteString)
parseAuthorizationHeader Nothing = Nothing
parseAuthorizationHeader (Just x) = case (B.split ' ' x) of
  ("Basic" : y : _) ->
    if B.length y == 0 then
      Nothing
    else
      let decodedValue=D.decode y in
        case decodedValue of
          Left e -> Nothing
          Right val ->
            case (B.split ':' val) of
              (user:pass:_) -> Just (user, pass)
              _ -> Nothing
  _ -> Nothing

testAuth :: Maybe (B.ByteString, B.ByteString) -> IO Bool
testAuth Nothing = return False
testAuth (Just (user,pass)) = return isValidUser
  where
   isValidUser = user == "test" && pass == "pass@123"
