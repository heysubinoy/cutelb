module Proxy.Routes where

import qualified Config.Types as Cfg
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)

matchRoute :: [Cfg.RouteConfig] -> Text -> Maybe Cfg.RouteConfig
matchRoute cfgRoutes reqPath =
  find matches cfgRoutes
  where
    matches r = (Cfg.path r) `T.isPrefixOf` reqPath
