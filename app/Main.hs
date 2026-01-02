{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server.HTTP (startHTTP)
import Config.Loader (loadConfig)
import Config.Validate (validateConfig)
import Runtime.Init (initRuntime)
import Log

import qualified Data.Text as T
import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.Exit (exitFailure)

main :: IO ()
main = do
  logInfo "=== cutelb - A Cute Load Balancer ==="
  
  args <- getArgs
  let configPath = case args of
        (p:_) -> p
        []    -> "config.yaml"

  logInfo $ "Loading configuration from: " <> T.pack configPath
  
  result <- catch (Right <$> loadConfig configPath) handleLoadError
  
  case result of
    Left err -> do
      logError $ "Failed to load configuration: " <> err
      exitFailure
    Right cfg -> do
      logInfo "Configuration loaded successfully"
      
      case validateConfig cfg of
        Left err -> do
          logError $ "Configuration validation failed: " <> err
          exitFailure
        Right validCfg -> do
          logInfo "Configuration validated successfully"
          runtime <- initRuntime validCfg
          startHTTP runtime

handleLoadError :: SomeException -> IO (Either T.Text a)
handleLoadError e = pure $ Left $ T.pack $ show e
