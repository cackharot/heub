{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core
where

import           Snap.Snaplet
import           Snap.Core
import qualified Data.ByteString.Char8 as B
import Control.Lens

import Api.UserService
import Api.InfoService

data Api = Api {
   _userService :: Snaplet UserService
 , _infoService :: Snaplet InfoService
}

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
        userService <- nestSnaplet "user" userService userServiceApiInit
        infoService <- nestSnaplet "" infoService infoServiceApiInit
        addRoutes apiRoutes
        return $ Api userService infoService
