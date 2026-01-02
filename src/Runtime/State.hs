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

data RuntimeUpstream = RuntimeUpstream
  { ruBackends :: [BackendConfig]
  , ruStrategy :: Strategy
  , ruCursor   :: TVar Int
  }

routesFromRuntime :: Runtime -> [RouteConfig]
routesFromRuntime = rtRoutes
