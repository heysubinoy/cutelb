{-# LANGUAGE OverloadedStrings #-}

module Runtime.Init where

import Config.Types (Config(..), UpstreamConfig(..), BackendConfig, weight, host, port)
import Runtime.State
import Log
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

initRuntime :: Config -> IO Runtime
initRuntime cfg = do
  logInfo "Initializing runtime..."
  ups <- atomically $ traverse initUpstream (upstreams cfg)
  tvar <- newTVarIO ups
  
  -- Log upstream and backend info
  mapM_ logUpstreamInfo (Map.toList ups)
  logInfo $ "Loaded " <> T.pack (show (length $ routes cfg)) <> " route(s)"
  
  pure $ Runtime tvar (routes cfg) (server cfg)

logUpstreamInfo :: (T.Text, RuntimeUpstream) -> IO ()
logUpstreamInfo (name, ru) = do
  let backendCount = V.length (ruBackends ru)
      strategyName = T.pack $ show $ ruStrategy ru
  logInfoCtx "Registered upstream"
    [ ("name", name)
    , ("backends", T.pack $ show backendCount)
    , ("strategy", strategyName)
    ]
  V.mapM_ (logBackendInfo name) (ruBackends ru)

logBackendInfo :: T.Text -> RuntimeBackend -> IO ()
logBackendInfo upstreamName rb = do
  let bc = rbConfig rb
      addr = host bc <> ":" <> T.pack (show $ port bc)
      w = fromMaybe 1 (weight bc)
  logDebug $ "  Backend: " <> addr <> " (weight=" <> T.pack (show w) <> ") upstream=" <> upstreamName

initUpstream :: UpstreamConfig -> STM RuntimeUpstream
initUpstream u = do
  cursor <- newTVar 0
  runtimeBackends <- traverse initBackend (servers u)
  let backendsVec = V.fromList runtimeBackends
      weightedVec = V.fromList $ concatMap expandBackend runtimeBackends
  pure RuntimeUpstream
    { ruBackends         = backendsVec
    , ruWeightedBackends = weightedVec
    , ruStrategy         = strategy u
    , ruCursor           = cursor
    }
  where
    expandBackend :: RuntimeBackend -> [RuntimeBackend]
    expandBackend rb = replicate (getWeight rb) rb

    getWeight :: RuntimeBackend -> Int
    getWeight rb = max 1 (fromMaybe 1 (weight (rbConfig rb)))

initBackend :: Config.Types.BackendConfig -> STM RuntimeBackend
initBackend bc = do
  conns <- newTVar 0
  respTime <- newTVar 0.0  -- start with 0, will be updated after first request
  pure RuntimeBackend
    { rbConfig       = bc
    , rbConnections  = conns
    , rbResponseTime = respTime
    }
