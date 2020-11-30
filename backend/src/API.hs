{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module API
    ( API
    , endpoints
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Semigroup ((<>))
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)
import Options.Applicative
import Servant

import Users as Users

type API =
    "signin" :> ReqBody '[JSON] Users.SignIn :> Post '[JSON] NoContent :<|>
    "joinnow" :> ReqBody '[JSON] Users.JoinNow :> Post '[JSON] NoContent

endpoints :: Server API
endpoints =
    signInH :<|>
    joinNowH
