{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Proxy.Forward where

import qualified Config.Types as Cfg
import Runtime.State (RuntimeBackend(..), rbConfig, rbConnections, rbResponseTime)
import Log
import qualified Network.Wai as Wai
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracket, try, SomeException)
import Control.Concurrent.STM
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Exponential moving average alpha (0.2 = 20% weight to new value)
emaAlpha :: Double
emaAlpha = 0.2

forwardRequest :: RuntimeBackend -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
forwardRequest rb waiReq respond = do
  startTime <- getCurrentTime
  let backendHost = Cfg.host (rbConfig rb)
      backendPort = Cfg.port (rbConfig rb)
      backendAddr = backendHost <> ":" <> T.pack (show backendPort)

  bracket
    (atomically $ modifyTVar' (rbConnections rb) (+ 1))
    (\_ -> do
      atomically $ modifyTVar' (rbConnections rb) (subtract 1)
      endTime <- getCurrentTime
      let elapsed = realToFrac (diffUTCTime endTime startTime) :: Double
      atomically $ modifyTVar' (rbResponseTime rb) (updateEMA elapsed)
      logDebug $ "Request completed: backend=" <> backendAddr <> " duration=" <> T.pack (show elapsed) <> "s"
    )
    (\_ -> doForward (rbConfig rb) waiReq respond backendAddr)

-- | Update exponential moving average
updateEMA :: Double -> Double -> Double
updateEMA newValue oldAvg
  | oldAvg == 0 = newValue  -- first request, use actual value
  | otherwise   = emaAlpha * newValue + (1 - emaAlpha) * oldAvg

doForward :: Cfg.BackendConfig -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> T.Text -> IO Wai.ResponseReceived
doForward backend waiReq respond backendAddr = do
  manager <- newManager defaultManagerSettings

  body <- Wai.strictRequestBody waiReq

  let url =
        "http://"
        <> T.unpack (Cfg.host backend)
        <> ":"
        <> show (Cfg.port backend)
        <> BS.unpack (Wai.rawPathInfo waiReq)

  result <- try $ do
    initReq <- parseRequest url
    let proxyReq =
          initReq
            { method = Wai.requestMethod waiReq
            , requestHeaders = Wai.requestHeaders waiReq
            , requestBody = RequestBodyLBS body
            }
    httpLbs proxyReq manager

  case result of
    Left (e :: SomeException) -> do
      logErrorCtx "Backend request failed"
        [ ("backend", backendAddr)
        , ("url", T.pack url)
        , ("error", T.pack $ show e)
        ]
      respond $
        Wai.responseLBS
          status502
          [("Content-Type", "application/json")]
          "{\"error\": \"Bad Gateway\", \"message\": \"Failed to connect to backend\"}"

    Right resp -> do
      let respStatus = responseStatus resp
      if statusCode respStatus >= 500
        then logWarnCtx "Backend returned error"
          [ ("backend", backendAddr)
          , ("status", T.pack $ show $ statusCode respStatus)
          ]
        else logDebug $ "Backend response: status=" <> T.pack (show $ statusCode respStatus)

      respond $
        Wai.responseLBS
          respStatus
          (responseHeaders resp)
          (responseBody resp)
