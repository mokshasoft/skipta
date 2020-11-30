{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module Users
    ( signInH
    , joinNowH
    , SignIn
    , JoinNow
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
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
