{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Config.Types where

import GHC.Generics
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Aeson

data ServerConfig = ServerConfig
  { listen :: Int
  , access_log :: Maybe FilePath
  , error_log :: Maybe FilePath
  } deriving (Show, Generic)

instance FromJSON ServerConfig


data UpstreamConfig = UpstreamConfig
  { strategy     :: Strategy
  , health_check :: Maybe Text
  , servers      :: [BackendConfig]
  } deriving (Show, Generic)

instance FromJSON UpstreamConfig

data BackendConfig = BackendConfig
  { host   :: Text
  , port   :: Int
  , weight :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON BackendConfig

data Strategy
  = RoundRobin
  | WeightedRoundRobin
  | LeastConn
  | Random
  | LeastResponseTime
  deriving (Show, Generic)

instance FromJSON Strategy where
  parseJSON = withText "Strategy" $ \case
    "round_robin"          -> pure RoundRobin
    "weighted_round_robin" -> pure WeightedRoundRobin
    "least_conn"           -> pure LeastConn
    "random"               -> pure Random
    "least_response_time"  -> pure LeastResponseTime
    _                      -> fail "unknown strategy (use: round_robin, weighted_round_robin, least_conn, random, least_response_time)"

data MatchType
  = MatchExact   -- Only matches the exact path
  | MatchPrefix  -- Matches if path starts with pattern
  | MatchRegex   -- Matches using regex pattern
  deriving (Show, Generic)

instance FromJSON MatchType where
  parseJSON = withText "MatchType" $ \case
    "exact"  -> pure MatchExact
    "prefix" -> pure MatchPrefix
    "regex"  -> pure MatchRegex
    _        -> fail "unknown match_type (use: exact, prefix, regex)"

data RouteConfig = RouteConfig
  { path       :: Text
  , upstream   :: Text
  , match_type :: MatchType
  } deriving (Show, Generic)

instance FromJSON RouteConfig

data Config = Config
  { server    :: ServerConfig
  , upstreams :: Map Text UpstreamConfig
  , routes    :: [RouteConfig]
  } deriving (Show, Generic)

instance FromJSON Config
