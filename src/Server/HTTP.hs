module Server.HTTP where

import Network.Wai.Handler.Warp
import Proxy.App
import Runtime.State


startHTTP :: Runtime -> IO ()
startHTTP runtime =
  run 8080 (proxyApp runtime)
