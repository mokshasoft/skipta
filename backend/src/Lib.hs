{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
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

configDefaultPort :: Int
configDefaultPort = 8080

data Opts = Opts
  { debug :: Bool
  , port  :: Int
  }

data SignIn = SignIn
  { salt :: String
  , password :: String
  } deriving (Eq)

instance Show SignIn where
  show (SignIn s p) = "SignIn {salt=" ++ show s ++ ", password=" ++ show p ++ "}"

$(deriveJSON defaultOptions ''SignIn)

type API =
    "signin" :> ReqBody '[JSON] SignIn :> Post '[JSON] NoContent

-- type Middleware = Application -> Application
corsPolicy :: Middleware
corsPolicy = cors (const $ Just policy)
    where
        policy = simpleCorsResourcePolicy
          { corsMethods = [ "GET", "POST", "PUT", "OPTIONS" ]
          , corsRequestHeaders = [ "content-type" ]
          }

options :: Parser Opts
options = Opts
    <$> switch
        ( long "debug"
        <> short 'd'
        <> help "Enable logging"
        )
    <*> option auto
        ( long "port"
        <> short 'p'
        <> help "Select which port to start on"
        <> showDefault
        <> value configDefaultPort
        <> metavar "PORT"
        )

startApp :: IO ()
startApp = start =<< execParser opts
  where
    opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "Start the skipta server"
        <> header "Skipta server - share costs"
        )

start :: Opts -> IO ()
start (Opts dbg port) = do
    liftIO $ putStrLn $ "Starting server on port " ++ show port
    withStdoutLogger $ \logger ->
        let log = if dbg then setLogger logger else id
        in runSettings (setPort port $ log $ defaultSettings) $ corsPolicy $ app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

signInH :: SignIn -> Handler NoContent
signInH json = do
    liftIO $ print json
    return NoContent

server :: Server API
server = signInH
