module Server.HTTP where

import Config.Types (ServerConfig(..))
import Network.Wai.Handler.Warp
import Proxy.App
import Runtime.State


startHTTP :: Runtime -> IO ()
startHTTP runtime =
  run (listen (rtServer runtime)) (proxyApp runtime)
