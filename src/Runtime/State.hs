module Runtime.State where

import Config.Types (BackendConfig, RouteConfig, ServerConfig, Strategy)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Network.HTTP.Client (Manager)

data Runtime = Runtime
  { rtUpstreams :: TVar (Map Text RuntimeUpstream)
  , rtRoutes    :: [RouteConfig]
  , rtServer    :: ServerConfig
  , rtManager   :: Manager  -- shared connection pool for proxying and health checks
  }

data RuntimeBackend = RuntimeBackend
  { rbConfig       :: BackendConfig
  , rbConnections  :: TVar Int
  , rbResponseTime :: TVar Double  -- exponential moving average of response time in seconds
  , rbHealthy      :: TVar Bool    -- updated by the health checker; starts True
  }

data RuntimeUpstream = RuntimeUpstream
  { ruBackends         :: Vector RuntimeBackend    -- O(1) indexing
  , ruWeightedBackends :: Vector RuntimeBackend    -- pre-computed expanded list for weighted RR
  , ruStrategy         :: Strategy
  , ruCursor           :: TVar Int
  , ruHealthCheck      :: Maybe Text               -- probe path, Nothing = checks disabled
  , ruHealthInterval   :: Int                      -- seconds between probes
  }

routesFromRuntime :: Runtime -> [RouteConfig]
routesFromRuntime = rtRoutes
