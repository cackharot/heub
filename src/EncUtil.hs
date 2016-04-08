{-# LANGUAGE OverloadedStrings #-}

module EncUtil (
    encryptMessage
  , decryptMessage
  )
where

import Control.Applicative
import Control.Monad

import Data.Maybe
import Crypto.Cipher.Types
import Crypto.Cipher
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as Base64

-- the bytestring need to have a length of 32 bytes
-- otherwise the simplified error handling will raise an exception.
initAES256 :: B.ByteString -> AES256
initAES256 = either (error . show) cipherInit . makeKey

-- real code would not create a new context every time, but
-- initialize once, and reuse the context.
encryptMessage :: B.ByteString -> B.ByteString
--encryptMessage msg = Base64.encode $ cbcEncrypt ctx iv msg
encryptMessage msg = Base64.encode $ ctrCombine ctx iv msg

decryptMessage :: B.ByteString -> B.ByteString
decryptMessage msg = ctrCombine ctx iv $ Base64.decodeLenient msg

ctx :: AES256
ctx = initAES256 key
  where
    key = B.pack "\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01\x01"

iv = fromMaybe (error "invalid IV") $ makeIV ivStr
  where
    ivStr = B.pack "\xC8\xA6\x45\x37\xA0\xB3\xA9\x3F\xCD\xE3\xCD\xAD\x9F\x1C\xE5\x8B"
