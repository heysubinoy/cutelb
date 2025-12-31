module Proxy.Forward where

import qualified Config.Types as Cfg
import qualified Network.Wai as Wai
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

forwardRequest :: Cfg.BackendConfig -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived
forwardRequest backend waiReq respond = do
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
