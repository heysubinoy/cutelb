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

forwardRequest :: Manager -> RuntimeBackend -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
forwardRequest manager rb waiReq respond = do
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
    (\_ -> doForward manager (rbConfig rb) waiReq respond backendAddr)

-- | Update exponential moving average
updateEMA :: Double -> Double -> Double
updateEMA newValue oldAvg
  | oldAvg == 0 = newValue  -- first request, use actual value
  | otherwise   = emaAlpha * newValue + (1 - emaAlpha) * oldAvg

-- | Hop-by-hop and framing headers that must not be copied between the two
-- connections; the HTTP client and Warp each set their own.
hopByHop :: [HeaderName]
hopByHop = ["Connection", "Keep-Alive", "Transfer-Encoding", "Content-Length", "Upgrade"]

stripHopByHop :: [Header] -> [Header]
stripHopByHop = filter (\(name, _) -> name `notElem` hopByHop)

doForward :: Manager -> Cfg.BackendConfig -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> T.Text -> IO Wai.ResponseReceived
doForward manager backend waiReq respond backendAddr = do
  body <- Wai.strictRequestBody waiReq

  let url =
        "http://"
        <> T.unpack (Cfg.host backend)
        <> ":"
        <> show (Cfg.port backend)
        <> BS.unpack (Wai.rawPathInfo waiReq <> Wai.rawQueryString waiReq)

  result <- try $ do
    initReq <- parseRequest url
    let proxyReq =
          initReq
            { method = Wai.requestMethod waiReq
            , requestHeaders = stripHopByHop (Wai.requestHeaders waiReq)
            , requestBody = RequestBodyLBS body
            , decompress = const False  -- pass bodies through byte-for-byte, matching Content-Encoding
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
          (stripHopByHop (responseHeaders resp))
          (responseBody resp)
