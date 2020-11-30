{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module API
    ( API
    , endpoints
    ) where

import Servant
import Users as Users

type API =
    "signin" :> ReqBody '[JSON] Users.SignIn :> Post '[JSON] NoContent :<|>
    "joinnow" :> ReqBody '[JSON] Users.JoinNow :> Post '[JSON] NoContent

endpoints :: Server API
endpoints =
    signInH :<|>
    joinNowH
