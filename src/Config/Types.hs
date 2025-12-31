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
  , workers :: Maybe Int
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
  | LeastConn
  deriving (Show, Generic)

instance FromJSON Strategy where
  parseJSON = withText "Strategy" $ \case
    "round_robin" -> pure RoundRobin
    "least_conn"  -> pure LeastConn
    _             -> fail "unknown strategy"

data RouteConfig = RouteConfig
  { path     :: Text
  , upstream :: Text
  } deriving (Show, Generic)

instance FromJSON RouteConfig

data Config = Config
  { server    :: ServerConfig
  , upstreams :: Map Text UpstreamConfig
  , routes    :: [RouteConfig]
  } deriving (Show, Generic)

instance FromJSON Config
