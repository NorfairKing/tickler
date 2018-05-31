{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server
    ( ticklerWebServer
    , makeTicklerApp
    ) where

import Import

import Control.Concurrent
import Control.Concurrent.Async (concurrently_)
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as Http

import Yesod

import Servant.Client (parseBaseUrl)


import qualified Tickler.Server as API
import qualified Tickler.Server.OptParse as API

import Tickler.Web.Server.Application ()
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse

ticklerWebServer :: IO ()
ticklerWebServer = do
    (DispatchServe ss, Settings) <- getInstructions
    putStrLn $
        unlines ["Running tickler-web-server with these settings:", ppShow ss]
    concurrently_ (runTicklerWebServer ss) (runTicklerAPIServer ss)

runTicklerWebServer :: ServeSettings -> IO ()
runTicklerWebServer ss@ServeSettings {..} = do
    appl <- makeTicklerApp ss
    warp serveSetPort appl

makeTicklerApp :: ServeSettings -> IO App
makeTicklerApp ServeSettings {..} = do
    man <- Http.newManager Http.defaultManagerSettings
    tokens <- newMVar HM.empty
    let apiPort = API.serveSetPort serveSetAPISettings
    burl <- parseBaseUrl $ "http://127.0.0.1:" ++ show apiPort
    pure
        App
        { appHttpManager = man
        , appStatic = myStatic
        , appPersistLogins = serveSetPersistLogins
        , appLoginTokens = tokens
        , appAPIBaseUrl = burl
        , appDefaultIntrayUrl = serveSetDefaultIntrayUrl
        }

runTicklerAPIServer :: ServeSettings -> IO ()
runTicklerAPIServer ss = API.runTicklerServer $ serveSetAPISettings ss
