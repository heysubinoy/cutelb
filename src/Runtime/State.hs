module Runtime.State where

import Config.Types (BackendConfig, RouteConfig, ServerConfig, Strategy)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)

data Runtime = Runtime
  { rtUpstreams :: TVar (Map Text RuntimeUpstream)
  , rtRoutes    :: [RouteConfig]
  , rtServer    :: ServerConfig
  }

data RuntimeBackend = RuntimeBackend
  { rbConfig       :: BackendConfig
  , rbConnections  :: TVar Int
  , rbResponseTime :: TVar Double  -- exponential moving average of response time in seconds
  }
data RuntimeUpstream = RuntimeUpstream
  { ruBackends         :: Vector RuntimeBackend    -- O(1) indexing
  , ruWeightedBackends :: Vector RuntimeBackend    -- pre-computed expanded list for weighted RR
  , ruStrategy         :: Strategy
  , ruCursor           :: TVar Int
  }

routesFromRuntime :: Runtime -> [RouteConfig]
routesFromRuntime = rtRoutes
