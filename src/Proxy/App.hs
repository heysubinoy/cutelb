{-# LANGUAGE OverloadedStrings #-}

module Proxy.App where

import Runtime.State
import Upstream.Strategy
import Proxy.Routes
import Proxy.Forward
import Config.Types (RouteConfig(..), host, port)
import Log

import Network.Wai
import Network.HTTP.Types

import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS


proxyApp :: Runtime -> Application
proxyApp runtime req respond = do
  let reqPath = TE.decodeUtf8 (rawPathInfo req)
      reqMethod = TE.decodeUtf8 (requestMethod req)
      clientHost = maybe "unknown" (TE.decodeUtf8 . fst) (remoteHost' req)

  logInfoCtx "Incoming request"
    [ ("method", reqMethod)
    , ("path", reqPath)
    , ("client", clientHost)
    ]

  rtUpstreamsMap <- atomically $ readTVar (rtUpstreams runtime)

  case matchRoute (routesFromRuntime runtime) reqPath of
    Nothing -> do
      logWarnCtx "No matching route found"
        [ ("path", reqPath)
        ]
      respond $
        responseLBS status404 [("Content-Type", "application/json")] 
          "{\"error\": \"Not Found\", \"message\": \"No route matches the requested path\"}"

    Just route -> do
      logDebug $ "Route matched: path=" <> reqPath <> " -> upstream=" <> upstream route

      case Map.lookup (upstream route) rtUpstreamsMap of
        Nothing -> do
          logErrorCtx "Upstream not found in configuration"
            [ ("upstream", upstream route)
            , ("path", reqPath)
            ]
          respond $
            responseLBS status502 [("Content-Type", "application/json")]
              "{\"error\": \"Bad Gateway\", \"message\": \"Upstream not found in configuration\"}"

        Just ru -> do
          mBackend <- pickBackend ru
          case mBackend of
            Nothing -> do
              logErrorCtx "No backends available for upstream"
                [ ("upstream", upstream route)
                ]
              respond $
                responseLBS status503 [("Content-Type", "application/json")]
                  "{\"error\": \"Service Unavailable\", \"message\": \"No healthy backends available\"}"
            Just backend -> do
              let backendHost = host (rbConfig backend)
                  backendPort = T.pack $ show $ port (rbConfig backend)
              logInfoCtx "Forwarding request to backend"
                [ ("upstream", upstream route)
                , ("backend", backendHost <> ":" <> backendPort)
                , ("strategy", T.pack $ show $ ruStrategy ru)
                ]
              forwardRequest backend req respond

-- Helper to get remote host (simplified)
remoteHost' :: Request -> Maybe (BS.ByteString, Int)
remoteHost' _ = Nothing  -- WAI doesn't expose this directly, would need socket info
