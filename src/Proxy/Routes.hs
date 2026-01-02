module Proxy.Routes where

import qualified Config.Types as Cfg
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find, sortBy)
import Data.Ord (comparing, Down(..))
import Text.Regex.TDFA ((=~))

matchRoute :: [Cfg.RouteConfig] -> Text -> Maybe Cfg.RouteConfig
matchRoute cfgRoutes reqPath =
  find matches sortedRoutes
  where
    -- Sort by priority: Exact > Regex > Prefix, then by path length
    sortedRoutes = sortBy routePriority cfgRoutes
    
    routePriority r1 r2 =
      case compare (matchTypePriority $ Cfg.match_type r1) (matchTypePriority $ Cfg.match_type r2) of
        EQ -> compare (Down $ T.length $ Cfg.path r1) (Down $ T.length $ Cfg.path r2)
        x  -> x
    
    matchTypePriority Cfg.MatchExact  = 0 :: Int
    matchTypePriority Cfg.MatchRegex  = 1
    matchTypePriority Cfg.MatchPrefix = 2
    
    matches r = case Cfg.match_type r of
      Cfg.MatchExact  -> Cfg.path r == reqPath
      Cfg.MatchPrefix -> Cfg.path r `T.isPrefixOf` reqPath
      Cfg.MatchRegex  -> T.unpack reqPath =~ T.unpack (Cfg.path r)
