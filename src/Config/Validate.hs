{-# LANGUAGE OverloadedStrings #-}

module Config.Validate where

import Config.Types (Config(..), RouteConfig(..), UpstreamConfig(..))
import Data.Text (Text)
import qualified Data.Map.Strict as Map

validateConfig :: Config -> Either Text Config
validateConfig cfg = do
  validateRoutes cfg
  validateUpstreams cfg
  Right cfg


validateRoutes :: Config -> Either Text ()
validateRoutes cfg =
  case filter invalid (routes cfg) of
    []    -> Right ()
    (r:_) -> Left ("Unknown upstream: " <> upstream r)
  where
    invalid r =
      not (Map.member (upstream r) (upstreams cfg))


validateUpstreams :: Config -> Either Text ()
validateUpstreams cfg =
  case filter empty (Map.elems (upstreams cfg)) of
    []    -> Right ()
    (_:_) -> Left "Upstream with no servers"
  where
    empty u = null (servers u)
