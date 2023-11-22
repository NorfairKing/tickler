{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server (ticklerWebServer) where

import Import
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Tickler.Web.Server.Application ()
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse
import Yesod

ticklerWebServer :: IO ()
ticklerWebServer = do
  ss <- getSettings
  putStrLn $ unlines ["Running tickler-web-server with these settings:", ppShow ss]
  runTicklerWebServer ss

runTicklerWebServer :: Settings -> IO ()
runTicklerWebServer Settings {..} = do
  man <- HTTP.newManager HTTP.tlsManagerSettings
  sessionKeyFile <- resolveFile' "client_session_key.aes"
  warp
    setPort
    App
      { appHTTPManager = man,
        appLogLevel = setLogLevel,
        appStatic = myStatic,
        appAPIBaseUrl = setAPIBaseUrl,
        appTracking = setTracking,
        appVerification = setVerification,
        appSessionKeyFile = sessionKeyFile,
        appDefaultIntrayUrl = setDefaultIntrayUrl
      }
