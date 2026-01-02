{-# LANGUAGE OverloadedStrings #-}

module Proxy.App where

import Runtime.State
import Upstream.Strategy
import Proxy.Routes
import Proxy.Forward
import Config.Types (RouteConfig(..))

import Network.Wai
import Network.HTTP.Types

import Control.Concurrent.STM
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map


proxyApp :: Runtime -> Application
proxyApp runtime req respond = do
  let reqPath = TE.decodeUtf8 (rawPathInfo req)

  rtUpstreamsMap <- atomically $ readTVar (rtUpstreams runtime)

  case matchRoute (routesFromRuntime runtime) reqPath of
    Nothing ->
      respond $
        responseLBS status404 [] "No matching route"

    Just route -> do
      case Map.lookup (upstream route) rtUpstreamsMap of
        Nothing ->
          respond $
            responseLBS status502 [] "Upstream not found"

        Just ru -> do
          backend <- pickBackend ru
          forwardRequest backend req respond
