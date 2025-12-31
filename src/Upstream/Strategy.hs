module Upstream.Strategy where

import Runtime.State
import Config.Types (BackendConfig, Strategy(..))
import Control.Concurrent.STM


pickBackend :: RuntimeUpstream -> STM BackendConfig
pickBackend ru =
  case ruStrategy ru of
    RoundRobin -> roundRobin ru
    LeastConn -> error "LeastConn not implemented yet"

roundRobin :: RuntimeUpstream -> STM BackendConfig
roundRobin ru = do
  let backends = ruBackends ru

  case backends of
    [] -> error "No backends available"
    _  -> do
      i <- readTVar (ruCursor ru)

      let idx = i `mod` length backends

      writeTVar (ruCursor ru) (i + 1)

          pure (backends !! idx)
