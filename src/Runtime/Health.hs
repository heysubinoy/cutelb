{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Active health checking.
--
-- For every upstream with a @health_check@ path, a background thread probes
-- each backend with @GET <path>@ every @health_interval@ seconds. A backend is
-- healthy when it answers 2xx/3xx within the timeout; anything else (error
-- status, refused connection, timeout) marks it unhealthy. Load-balancing
-- strategies only pick healthy backends, and a backend rejoins the pool as
-- soon as a probe succeeds again.
module Runtime.Health (startHealthChecks) where

import qualified Config.Types as Cfg
import Runtime.State
import Log

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, void, when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Types (statusCode)

-- | Per-probe timeout.
probeTimeoutMicros :: Int
probeTimeoutMicros = 2 * 1000000

-- | Fork one checker thread per upstream that has a health_check path.
startHealthChecks :: Runtime -> IO ()
startHealthChecks runtime = do
  ups <- readTVarIO (rtUpstreams runtime)
  forM_ (Map.toList ups) $ \(name, ru) ->
    case ruHealthCheck ru of
      Nothing -> pure ()
      Just hcPath -> do
        logInfoCtx "Starting health checks"
          [ ("upstream", name)
          , ("path", hcPath)
          , ("interval", T.pack (show (ruHealthInterval ru)) <> "s")
          ]
        void $ forkIO $ forever $ do
          V.mapM_ (probe (rtManager runtime) name hcPath) (ruBackends ru)
          threadDelay (ruHealthInterval ru * 1000000)

-- | Probe one backend and record the result, logging only on state changes.
probe :: Manager -> Text -> Text -> RuntimeBackend -> IO ()
probe manager upstreamName hcPath rb = do
  let bc   = rbConfig rb
      addr = Cfg.host bc <> ":" <> T.pack (show (Cfg.port bc))
      url  = "http://" <> T.unpack addr <> T.unpack hcPath

  result <- try $ do
    req <- parseRequest url
    resp <- httpNoBody req { responseTimeout = responseTimeoutMicro probeTimeoutMicros } manager
    pure (statusCode (responseStatus resp))

  let healthy = case result of
        Right code              -> code >= 200 && code < 400
        Left (_ :: SomeException) -> False

  wasHealthy <- atomically $ swapTVar (rbHealthy rb) healthy
  when (wasHealthy /= healthy) $
    if healthy
      then logInfoCtx "Backend recovered" [("upstream", upstreamName), ("backend", addr)]
      else logWarnCtx "Backend marked unhealthy"
             [ ("upstream", upstreamName)
             , ("backend", addr)
             , ("reason", either (T.pack . show) (\c -> "status " <> T.pack (show c)) result)
             ]
