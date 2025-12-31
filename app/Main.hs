module Main where

import Server.HTTP (startHTTP)
import Config.Loader (loadConfig)
import Config.Validate (validateConfig)
import Runtime.Init (initRuntime)

import Data.Text (unpack)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let configPath = case args of
        (p:_) -> p
        []    -> "config.yaml"

  cfg <- loadConfig configPath

  case validateConfig cfg of
    Left err -> error (unpack err)
    Right ok -> do
      runtime <- initRuntime ok
      startHTTP runtime
