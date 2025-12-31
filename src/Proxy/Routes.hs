module Proxy.Routes where

import Config.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)

matchRoute :: [RouteConfig] -> Text -> Maybe RouteConfig
matchRoute routes path =
  find matches routes
  where
    matches r = path `T.isPrefixOf` pathOf r
    pathOf = path
