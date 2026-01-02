module Upstream.Strategy where

import Runtime.State
import Config.Types (Strategy(..))
import Control.Concurrent.STM
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Random (randomRIO)
import qualified Data.Vector as V


-- | Pick a backend based on the configured strategy.
-- Returns Nothing if no backends are available.
pickBackend :: RuntimeUpstream -> IO (Maybe RuntimeBackend)
pickBackend ru =
  case ruStrategy ru of
    RoundRobin         -> atomically $ roundRobin ru
    WeightedRoundRobin -> atomically $ weightedRoundRobin ru
    LeastConn          -> atomically $ leastConn ru
    Random             -> randomBackend ru
    LeastResponseTime  -> leastResponseTime ru

-- | Simple round robin: each backend is selected in turn, ignoring weights.
roundRobin :: RuntimeUpstream -> STM (Maybe RuntimeBackend)
roundRobin ru
  | V.null backends = pure Nothing
  | otherwise = do
      i <- readTVar (ruCursor ru)
      let idx = i `mod` V.length backends
      writeTVar (ruCursor ru) (i + 1)
      pure $ Just (backends V.! idx)
  where
    backends = ruBackends ru

-- | Weighted round robin: backends are selected proportionally to their weights.
-- Uses pre-computed expanded list for efficiency.
weightedRoundRobin :: RuntimeUpstream -> STM (Maybe RuntimeBackend)
weightedRoundRobin ru
  | V.null expanded = pure Nothing
  | otherwise = do
      i <- readTVar (ruCursor ru)
      let idx = i `mod` V.length expanded
      writeTVar (ruCursor ru) (i + 1)
      pure $ Just (expanded V.! idx)
  where
    expanded = ruWeightedBackends ru

-- | Least connections: select the backend with the fewest active connections.
leastConn :: RuntimeUpstream -> STM (Maybe RuntimeBackend)
leastConn ru
  | V.null (ruBackends ru) = pure Nothing
  | otherwise = do
      backendCounts <- mapM withConnCount (V.toList $ ruBackends ru)
      pure $ Just $ snd $ minimumBy (comparing fst) backendCounts
  where
    withConnCount rb = do
      count <- readTVar (rbConnections rb)
      pure (count, rb)

-- | Random: pick a backend at random.
randomBackend :: RuntimeUpstream -> IO (Maybe RuntimeBackend)
randomBackend ru
  | V.null backends = pure Nothing
  | otherwise = do
      idx <- randomRIO (0, V.length backends - 1)
      pure $ Just (backends V.! idx)
  where
    backends = ruBackends ru

-- | Least response time: select the backend with the lowest average response time.
-- Backends with 0 response time (untested) are selected via round-robin to warm them up.
leastResponseTime :: RuntimeUpstream -> IO (Maybe RuntimeBackend)
leastResponseTime ru
  | V.null (ruBackends ru) = pure Nothing
  | otherwise = do
      backendTimes <- atomically $ mapM withRespTime (V.toList $ ruBackends ru)
      let (untested, tested) = partition (\(rt, _) -> rt == 0.0) backendTimes
      case untested of
        -- If there are untested backends, use round-robin among them
        (_:_) -> atomically $ do
          i <- readTVar (ruCursor ru)
          let untestedVec = V.fromList (map snd untested)
              idx = i `mod` V.length untestedVec
          writeTVar (ruCursor ru) (i + 1)
          pure $ Just (untestedVec V.! idx)
        -- All backends tested, pick the one with lowest response time
        [] -> pure $ Just $ snd $ minimumBy (comparing fst) tested
  where
    withRespTime rb = do
      rt <- readTVar (rbResponseTime rb)
      pure (rt, rb)

    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition p xs = (filter p xs, filter (not . p) xs)
