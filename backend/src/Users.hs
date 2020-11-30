{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Users
    ( signInH
    , joinNowH
    , SignIn
    , JoinNow
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

data SignIn = SignIn
  { salt :: String
  , password :: String
  } deriving (Eq)

data JoinNow = JoinNow
  { salt :: String
  , password :: String
  } deriving (Eq)

instance Show SignIn where
  show (SignIn s p) = "SignIn {salt=" ++ show s ++ ", password=" ++ show p ++ "}"

instance Show JoinNow where
  show (JoinNow s p) = "JoinNow {salt=" ++ show s ++ ", password=" ++ show p ++ "}"

$(deriveJSON defaultOptions ''SignIn)
$(deriveJSON defaultOptions ''JoinNow)

signInH :: SignIn -> Handler NoContent
signInH json = do
    liftIO $ print json
    return NoContent

joinNowH :: JoinNow -> Handler NoContent
joinNowH json = do
    liftIO $ print json
    return NoContent
