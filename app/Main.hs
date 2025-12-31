import Server.HTTP
startHTTP runtime

main :: IO ()
main = do
  opts <- parseOptions
  cfg  <- loadConfig (configPath opts)

  case validateConfig cfg of
    Left err -> error (unpack err)
    Right ok -> do
      runtime <- initRuntime ok
      startHTTP runtime
