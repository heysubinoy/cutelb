module Runtime.Init where

import Config.Types (Config(..), UpstreamConfig(..), BackendConfig)
import Runtime.State
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

initRuntime :: Config -> IO Runtime
initRuntime cfg = do
  ups <- atomically $ traverse initUpstream (upstreams cfg)
  tvar <- newTVarIO ups
  pure $ Runtime tvar (routes cfg) (server cfg)

initUpstream :: UpstreamConfig -> STM RuntimeUpstream
initUpstream u = do
  cursor <- newTVar 0
  runtimeBackends <- traverse initBackend (servers u)
  pure RuntimeUpstream
    { ruBackends = runtimeBackends
    , ruStrategy = strategy u
    , ruCursor   = cursor
    }

initBackend :: Config.Types.BackendConfig -> STM RuntimeBackend
initBackend bc = do
  conns <- newTVar 0
  respTime <- newTVar 0.0  -- start with 0, will be updated after first request
  pure RuntimeBackend
    { rbConfig       = bc
    , rbConnections  = conns
    , rbResponseTime = respTime
    }
