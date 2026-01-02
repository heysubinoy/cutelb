module Upstream.Strategy where

import Runtime.State
import Config.Types (BackendConfig, Strategy(..), weight)
import Control.Concurrent.STM
import Data.Maybe (fromMaybe)
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Random (randomRIO)


pickBackend :: RuntimeUpstream -> IO RuntimeBackend
pickBackend ru =
  case ruStrategy ru of
    RoundRobin         -> atomically $ roundRobin ru
    WeightedRoundRobin -> atomically $ weightedRoundRobin ru
    LeastConn          -> atomically $ leastConn ru
    Random             -> randomBackend ru
    LeastResponseTime  -> atomically $ leastResponseTime ru

-- | Simple round robin: each backend is selected in turn, ignoring weights.
roundRobin :: RuntimeUpstream -> STM RuntimeBackend
roundRobin ru = do
  let backends = ruBackends ru

  case backends of
    [] -> error "No backends available"
    _  -> do
      i <- readTVar (ruCursor ru)
      let idx = i `mod` length backends
      writeTVar (ruCursor ru) (i + 1)
      pure (backends !! idx)

-- | Weighted round robin: backends are selected proportionally to their weights.
-- A backend with weight 3 will be selected 3 times before moving to the next.
-- Default weight is 1 if not specified.
weightedRoundRobin :: RuntimeUpstream -> STM RuntimeBackend
weightedRoundRobin ru = do
  let backends = ruBackends ru
      -- Expand backends according to their weights
      expanded = concatMap expandBackend backends

  case expanded of
    [] -> error "No backends available"
    _  -> do
      i <- readTVar (ruCursor ru)
      let idx = i `mod` length expanded
      writeTVar (ruCursor ru) (i + 1)
      pure (expanded !! idx)
  where
    expandBackend :: RuntimeBackend -> [RuntimeBackend]
    expandBackend rb = replicate (getWeight rb) rb

    getWeight :: RuntimeBackend -> Int
    getWeight rb = max 1 (fromMaybe 1 (weight (rbConfig rb)))

-- | Least connections: select the backend with the fewest active connections.
leastConn :: RuntimeUpstream -> STM RuntimeBackend
leastConn ru = do
  backends <- mapM withConnCount (ruBackends ru)
  case backends of
    [] -> error "No backends available"
    _  -> pure $ snd $ minimumBy (comparing fst) backends
  where
    withConnCount rb = do
      count <- readTVar (rbConnections rb)
      pure (count, rb)

-- | Random: pick a backend at random.
randomBackend :: RuntimeUpstream -> IO RuntimeBackend
randomBackend ru = do
  let backends = ruBackends ru
  case backends of
    [] -> error "No backends available"
    _  -> do
      idx <- randomRIO (0, length backends - 1)
      pure (backends !! idx)

-- | Least response time: select the backend with the lowest average response time.
-- Backends with 0 response time (no requests yet) are prioritized.
leastResponseTime :: RuntimeUpstream -> STM RuntimeBackend
leastResponseTime ru = do
  backends <- mapM withRespTime (ruBackends ru)
  case backends of
    [] -> error "No backends available"
    _  -> pure $ snd $ minimumBy (comparing fst) backends
  where
    withRespTime rb = do
      rt <- readTVar (rbResponseTime rb)
      pure (rt, rb)
