module Proxy.Forward where

import qualified Config.Types as Cfg
import Runtime.State (RuntimeBackend(..), rbConfig, rbConnections, rbResponseTime)
import qualified Network.Wai as Wai
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracket_)
import Control.Concurrent.STM
import Data.Time.Clock (getCurrentTime, diffUTCTime)

-- | Exponential moving average alpha (0.2 = 20% weight to new value)
emaAlpha :: Double
emaAlpha = 0.2

forwardRequest :: RuntimeBackend -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
forwardRequest rb waiReq respond = do
  startTime <- getCurrentTime
  bracket_
    (atomically $ modifyTVar' (rbConnections rb) (+ 1))
    (do
      atomically $ modifyTVar' (rbConnections rb) (subtract 1)
      endTime <- getCurrentTime
      let elapsed = realToFrac (diffUTCTime endTime startTime) :: Double
      atomically $ modifyTVar' (rbResponseTime rb) (updateEMA elapsed)
    )
    (doForward (rbConfig rb) waiReq respond)

-- | Update exponential moving average
updateEMA :: Double -> Double -> Double
updateEMA newValue oldAvg
  | oldAvg == 0 = newValue  -- first request, use actual value
  | otherwise   = emaAlpha * newValue + (1 - emaAlpha) * oldAvg

doForward :: Cfg.BackendConfig -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
doForward backend waiReq respond = do
  manager <- newManager defaultManagerSettings

  body <- Wai.strictRequestBody waiReq

  let url =
        "http://"
        <> T.unpack (Cfg.host backend)
        <> ":"
        <> show (Cfg.port backend)
        <> BS.unpack (Wai.rawPathInfo waiReq)

  initReq <- parseRequest url

  let proxyReq =
        initReq
          { method = Wai.requestMethod waiReq
          , requestHeaders = Wai.requestHeaders waiReq
          , requestBody = RequestBodyLBS body
          }

  resp <- httpLbs proxyReq manager

  respond $
    Wai.responseLBS
      (responseStatus resp)
      (responseHeaders resp)
      (responseBody resp)
