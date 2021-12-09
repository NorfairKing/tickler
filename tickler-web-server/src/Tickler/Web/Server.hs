{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server
  ( ticklerWebServer,
    makeTicklerApp,
  )
where

import Control.Concurrent
import qualified Data.HashMap.Strict as HM
import Import
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import Tickler.Web.Server.Application ()
import Tickler.Web.Server.BootCheck
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse
import Yesod

ticklerWebServer :: IO ()
ticklerWebServer = do
  ss <- getSettings
  putStrLn $ unlines ["Running tickler-web-server with these settings:", ppShow ss]
  bootCheck (setDefaultIntrayUrl ss)
  runTicklerWebServer ss

runTicklerWebServer :: Settings -> IO ()
runTicklerWebServer ss@Settings {..} = do
  appl <- makeTicklerApp ss
  warp setPort appl

makeTicklerApp :: Settings -> IO App
makeTicklerApp Settings {..} = do
  man <- HTTP.newManager HTTP.tlsManagerSettings
  tokens <- newMVar HM.empty
  pure
    App
      { appHTTPManager = man,
        appStatic = myStatic,
        appLoginTokens = tokens,
        appAPIBaseUrl = setAPIBaseUrl,
        appTracking = setTracking,
        appVerification = setVerification,
        appPersistLogins = setPersistLogins,
        appDefaultIntrayUrl = setDefaultIntrayUrl
      }
