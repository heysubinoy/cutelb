module Runtime.State where

import Config.Types (BackendConfig, RouteConfig, ServerConfig, Strategy)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Text (Text)

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
  { ruBackends :: [RuntimeBackend]
  , ruStrategy :: Strategy
  , ruCursor   :: TVar Int
  }

routesFromRuntime :: Runtime -> [RouteConfig]
routesFromRuntime = rtRoutes
