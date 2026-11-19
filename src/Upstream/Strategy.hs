module Upstream.Strategy where

import Runtime.State
import Config.Types (Strategy(..))
import Control.Concurrent.STM
import Data.List (minimumBy, partition)
import Data.Ord (comparing)
import System.Random (randomRIO)
import qualified Data.Vector as V


-- | Pick a backend based on the configured strategy.
-- Only healthy backends are considered; returns Nothing if none are available.
pickBackend :: RuntimeUpstream -> IO (Maybe RuntimeBackend)
pickBackend ru =
  case ruStrategy ru of
    RoundRobin         -> atomically $ roundRobin ru
    WeightedRoundRobin -> atomically $ weightedRoundRobin ru
    LeastConn          -> atomically $ leastConn ru
    Random             -> randomBackend ru
    LeastResponseTime  -> leastResponseTime ru

-- | Backends currently marked healthy by the health checker.
healthyOnly :: V.Vector RuntimeBackend -> STM (V.Vector RuntimeBackend)
healthyOnly = V.filterM (readTVar . rbHealthy)

-- | Advance the shared cursor and pick from the given candidates.
cycleThrough :: RuntimeUpstream -> V.Vector RuntimeBackend -> STM (Maybe RuntimeBackend)
cycleThrough ru candidates
  | V.null candidates = pure Nothing
  | otherwise = do
      i <- readTVar (ruCursor ru)
      writeTVar (ruCursor ru) (i + 1)
      pure $ Just (candidates V.! (i `mod` V.length candidates))

-- | Simple round robin: each healthy backend is selected in turn, ignoring weights.
roundRobin :: RuntimeUpstream -> STM (Maybe RuntimeBackend)
roundRobin ru = healthyOnly (ruBackends ru) >>= cycleThrough ru

-- | Weighted round robin: healthy backends are selected proportionally to their weights.
-- Uses the pre-computed expanded list for efficiency.
weightedRoundRobin :: RuntimeUpstream -> STM (Maybe RuntimeBackend)
weightedRoundRobin ru = healthyOnly (ruWeightedBackends ru) >>= cycleThrough ru

-- | Least connections: select the healthy backend with the fewest active connections.
leastConn :: RuntimeUpstream -> STM (Maybe RuntimeBackend)
leastConn ru = do
  candidates <- healthyOnly (ruBackends ru)
  if V.null candidates
    then pure Nothing
    else do
      counts <- mapM withConnCount (V.toList candidates)
      pure $ Just $ snd $ minimumBy (comparing fst) counts
  where
    withConnCount rb = do
      count <- readTVar (rbConnections rb)
      pure (count, rb)

-- | Random: pick a healthy backend at random.
randomBackend :: RuntimeUpstream -> IO (Maybe RuntimeBackend)
randomBackend ru = do
  candidates <- atomically $ healthyOnly (ruBackends ru)
  if V.null candidates
    then pure Nothing
    else do
      idx <- randomRIO (0, V.length candidates - 1)
      pure $ Just (candidates V.! idx)

-- | Least response time: select the healthy backend with the lowest average response time.
-- Backends with no measurements yet are tried first, round-robin, to warm them up.
leastResponseTime :: RuntimeUpstream -> IO (Maybe RuntimeBackend)
leastResponseTime ru = atomically $ do
  candidates <- healthyOnly (ruBackends ru)
  timed <- mapM withRespTime (V.toList candidates)
  let (untested, tested) = partition (\(rt, _) -> rt == 0.0) timed
  case (untested, tested) of
    ([], [])  -> pure Nothing
    ((_:_), _) -> cycleThrough ru (V.fromList (map snd untested))
    ([], _)   -> pure $ Just $ snd $ minimumBy (comparing fst) tested
  where
    withRespTime rb = do
      rt <- readTVar (rbResponseTime rb)
      pure (rt, rb)
