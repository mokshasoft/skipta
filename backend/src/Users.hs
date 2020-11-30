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
  { email :: String
  , hashedPassword :: String
  } deriving (Eq)

data JoinNow = JoinNow
  { email :: String
  , hashedPassword :: String
  } deriving (Eq)

instance Show SignIn where
  show (SignIn s p) = "SignIn {email=" ++ show s ++ ", hashedPassword=" ++ show p ++ "}"

instance Show JoinNow where
  show (JoinNow s p) = "JoinNow {email=" ++ show s ++ ", hashedPassword=" ++ show p ++ "}"

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
