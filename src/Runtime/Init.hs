module Runtime.Init where

import Config.Types (Config(..), UpstreamConfig(..), BackendConfig, weight)
import Runtime.State
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

initRuntime :: Config -> IO Runtime
initRuntime cfg = do
  ups <- atomically $ traverse initUpstream (upstreams cfg)
  tvar <- newTVarIO ups
  pure $ Runtime tvar (routes cfg) (server cfg)

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
