module Config.Loader where

import Config.Types
import Data.Yaml

loadConfig :: FilePath -> IO Config
loadConfig path = do
    result <- decodeFileEither path
    case result of
        Left err -> error (prettyPrintParseException err)
        Right cfg -> pure cfg
