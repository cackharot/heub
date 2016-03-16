module Services.AuthenticationService
where

import App.Model

validateUser :: String -> String -> Bool
validateUser "" _ = False
validateUser _ "" = False
validateUser "" "" = False
validateUser username password = username == "admin" && password == "pass"

createUser :: User -> Bool
createUser user = True
