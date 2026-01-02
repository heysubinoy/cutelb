{-# LANGUAGE OverloadedStrings #-}

module Server.HTTP where

import Config.Types (ServerConfig(..))
import Network.Wai.Handler.Warp
import Proxy.App
import Runtime.State
import Log
import qualified Data.Text as T


startHTTP :: Runtime -> IO ()
startHTTP runtime = do
  let listenPort = listen (rtServer runtime)
  logInfo $ "Starting cutelb on port " <> T.pack (show listenPort)
  logInfo "Load balancer ready to accept connections"
  run listenPort (proxyApp runtime)
