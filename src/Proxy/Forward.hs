module Proxy.Forward where

import Config.Types
import Network.Wai
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS

forwardRequest :: BackendConfig -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
forwardRequest backend req respond = do
  manager <- newManager defaultManagerSettings

  body <- strictRequestBody req

  let url =
        "http://"
        <> show (host backend)
        <> ":"
        <> show (port backend)
        <> show (rawPathInfo req)

  initReq <- parseRequest url

  let proxyReq =
        initReq
          { method = requestMethod req
          , requestHeaders = requestHeaders req
          , requestBody = RequestBodyLBS body
          }

  resp <- httpLbs proxyReq manager

  respond $
    responseLBS
      (responseStatus resp)
      (responseHeaders resp)
      (responseBody resp)
