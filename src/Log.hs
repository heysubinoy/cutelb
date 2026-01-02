{-# LANGUAGE OverloadedStrings #-}

module Log where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.IO (hFlush, stdout, stderr, Handle)
import Control.Concurrent (myThreadId)

data LogLevel = DEBUG | INFO | WARN | ERROR
  deriving (Show, Eq, Ord)

-- | Format: [2026-01-02 15:04:05] [INFO] [ThreadId 42] Message
logWithLevel :: LogLevel -> Text -> IO ()
logWithLevel level msg = do
  now <- getCurrentTime
  tid <- myThreadId
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
      levelStr = T.pack $ show level
      tidStr = T.pack $ show tid
      formatted = T.concat ["[", timestamp, "] [", levelStr, "] [", tidStr, "] ", msg]
      handle = if level >= WARN then stderr else stdout
  TIO.hPutStrLn handle formatted
  hFlush handle

logDebug :: Text -> IO ()
logDebug = logWithLevel DEBUG

logInfo :: Text -> IO ()
logInfo = logWithLevel INFO

logWarn :: Text -> IO ()
logWarn = logWithLevel WARN

logError :: Text -> IO ()
logError = logWithLevel ERROR

-- | Log with key-value context
logInfoCtx :: Text -> [(Text, Text)] -> IO ()
logInfoCtx msg ctx = logInfo $ msg <> " " <> formatCtx ctx

logWarnCtx :: Text -> [(Text, Text)] -> IO ()
logWarnCtx msg ctx = logWarn $ msg <> " " <> formatCtx ctx

logErrorCtx :: Text -> [(Text, Text)] -> IO ()
logErrorCtx msg ctx = logError $ msg <> " " <> formatCtx ctx

formatCtx :: [(Text, Text)] -> Text
formatCtx pairs = T.intercalate " " $ map (\(k, v) -> k <> "=" <> v) pairs
