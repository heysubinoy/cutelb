module Runtime.Init where

import Config.Types (Config(..), UpstreamConfig(..))
import Runtime.State
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map

initRuntime :: Config -> IO Runtime
initRuntime cfg = do
  ups <- atomically $ traverse initUpstream (upstreams cfg)
  tvar <- newTVarIO ups
  pure $ Runtime tvar (routes cfg)

initUpstream :: UpstreamConfig -> STM RuntimeUpstream
initUpstream u = do
  cursor <- newTVar 0
  pure RuntimeUpstream
    { ruBackends = servers u
    , ruStrategy = strategy u
    , ruCursor   = cursor
    }
