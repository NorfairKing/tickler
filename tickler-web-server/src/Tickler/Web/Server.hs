{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server (ticklerWebServer) where

import Control.Concurrent
import qualified Data.HashMap.Strict as HM
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
  tokens <- newMVar HM.empty
  sessionKeyFile <- resolveFile' "client_session_key.aes"
  warp
    setPort
    App
      { appHTTPManager = man,
        appStatic = myStatic,
        appLoginTokens = tokens,
        appAPIBaseUrl = setAPIBaseUrl,
        appTracking = setTracking,
        appVerification = setVerification,
        appPersistLogins = setPersistLogins,
        appSessionKeyFile = sessionKeyFile,
        appDefaultIntrayUrl = setDefaultIntrayUrl
      }
