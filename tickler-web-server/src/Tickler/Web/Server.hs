{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server
    ( ticklerWebServer
    , makeTicklerApp
    ) where

import Import

import Control.Concurrent
import Control.Concurrent.Async (concurrently_)
import Control.Monad.Trans.AWS as AWS
import qualified Data.HashMap.Strict as HM
import qualified Network.HTTP.Client as Http

import Yesod

import Servant.Client (parseBaseUrl)

import Tickler.Data

import qualified Tickler.Server as API
import qualified Tickler.Server.OptParse as API

import Tickler.Web.Server.Application ()
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse

ticklerWebServer :: IO ()
ticklerWebServer = do
    (DispatchServe ss, Settings) <- getInstructions
    putStrLn $ ppShow ss
    concurrently_ (runTicklerWebServer ss) (runTicklerAPIServer ss)

runTicklerWebServer :: ServeSettings -> IO ()
runTicklerWebServer ss@ServeSettings {..} = do
    appl <- makeTicklerApp ss
    warp serveSetPort appl

makeTicklerApp :: ServeSettings -> IO App
makeTicklerApp ServeSettings {..} = do
    man <- Http.newManager Http.defaultManagerSettings
    tokens <- newMVar HM.empty
    burl <- parseBaseUrl $ "http://127.0.0.1:" ++ show serveSetAPIPort
    pure
        App
        { appHttpManager = man
        , appStatic = myStatic
        , appPersistLogins = serveSetPersistLogins
        , appLoginTokens = tokens
        , appAPIBaseUrl = burl
        , appDefaultIntrayUrl = serveSetDefaultIntrayUrl
        }

makeTicklerAPIServeSettings :: ServeSettings -> API.ServeSettings
makeTicklerAPIServeSettings ServeSettings {..} =
    API.ServeSettings
    { API.serveSetPort = serveSetAPIPort
    , API.serveSetConnectionInfo = serveSetAPIConnectionInfo
    , API.serveSetConnectionCount = serveSetAPIConnectionCount
    , API.serveSetAdmins = serveSetAPIAdmins
    , API.serveSetLooperSettings =
          API.LooperSettings
          { API.looperSetConnectionInfo = serveSetAPIConnectionInfo
          , API.looperSetConnectionCount = serveSetAPIConnectionCount
          , API.looperSetTriggerSets = API.LooperEnabled 5 API.TriggerSettings
          , API.looperSetEmailerSets = API.LooperDisabled
          , API.looperSetTriggeredIntrayItemSchedulerSets =
                API.LooperEnabled 5 ()
          , API.looperSetTriggeredIntrayItemSenderSets = API.LooperEnabled 5 ()
          , API.looperSetVerificationEmailConverterSets = API.LooperEnabled 5 ()
          , API.looperSetTriggeredEmailSchedulerSets = API.LooperEnabled 5 ()
          , API.looperSetTriggeredEmailConverterSets =
                API.LooperEnabled
                    5
                    API.TriggeredEmailConverterSettings
                    { triggeredEmailConverterSetFromAddress =
                          unsafeEmailAddress "tickler" "cs-syd.eu"
                    , triggeredEmailConverterSetFromName = "Tickler"
                    }
          }
    }

runTicklerAPIServer :: ServeSettings -> IO ()
runTicklerAPIServer ss = do
    let apiServeSets = makeTicklerAPIServeSettings ss
    API.runTicklerServer apiServeSets
