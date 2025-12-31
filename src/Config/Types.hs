{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Types where

import GHC.Generics
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Aeson

data ServerConfig = ServerConfig {
    listen :: Int
    , workers :: Maybe Int
    ,
}
