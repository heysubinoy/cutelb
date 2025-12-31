module Proxy.App where

import Runtime.State
import Upstream.Strategy
import Proxy.Routes

import Network.Wai
import Network.HTTP.Types
import Network.HTTP.Client

import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map


proxyApp :: Runtime -> Application
proxyApp runtime req respond = do
  let path = TE.decodeUtf8 (rawPathInfo req)

  rtUpstreamsMap <- atomically $ readTVar (rtUpstreams runtime)

  case matchRoute (routesFromRuntime runtime) path of
    Nothing ->
      respond $
        responseLBS status404 [] "No matching route"

    Just route -> do
      case Map.lookup (upstream route) rtUpstreamsMap of
        Nothing ->
          respond $
            responseLBS status502 [] "Upstream not found"

        Just ru -> do
          backend <- atomically $ pickBackend ru
          forwardRequest backend req respond
