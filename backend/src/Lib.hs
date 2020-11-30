{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Semigroup ((<>))
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Network.Wai.Handler.Warp (runSettings, setPort, setLogger, defaultSettings)
import Network.Wai.Logger (withStdoutLogger)
import Options.Applicative
import Servant

import API as API

configDefaultPort :: Int
configDefaultPort = 8080

data Opts = Opts
  { debug :: Bool
  , port  :: Int
  }

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

server :: Server API.API
server = API.endpoints

api :: Proxy API.API
api = Proxy

app :: Application
app = serve api server

start :: Opts -> IO ()
start (Opts dbg port) = do
    liftIO $ putStrLn $ "Starting server on port " ++ show port
    withStdoutLogger $ \logger ->
        let log = if dbg then setLogger logger else id
        in runSettings (setPort port $ log $ defaultSettings) $ corsPolicy $ app

startApp :: IO ()
startApp = start =<< execParser opts
  where
    opts = info (options <**> helper)
        ( fullDesc
        <> progDesc "Start the skipta server"
        <> header "Skipta server - share costs"
        )
