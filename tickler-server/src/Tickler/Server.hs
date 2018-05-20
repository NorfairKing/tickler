{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server
    ( runTicklerServer
    , makeTicklerServer
    , ticklerAppContext
    ) where

import Import

import Control.Concurrent.Async (concurrently_)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic

import Tickler.API
import Tickler.Data

import Tickler.Server.OptParse.Types
import Tickler.Server.Types

import Tickler.Server.Handler (ticklerServer)
import Tickler.Server.Looper
import Tickler.Server.SigningKey

runTicklerServer :: ServeSettings -> IO ()
runTicklerServer sets@ServeSettings {..} = do
    runStderrLoggingT $
        withSqlitePoolInfo serveSetConnectionInfo serveSetConnectionCount $ \pool ->
            runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    concurrently_ (runServer_ sets) (runLoopers_ sets)

runServer_ :: ServeSettings -> IO ()
runServer_ ServeSettings {..} = do
    runStderrLoggingT $
        withSqlitePoolInfo serveSetConnectionInfo serveSetConnectionCount $ \pool -> do
            signingKey <- liftIO loadSigningKey
            let jwtCfg = defaultJWTSettings signingKey
            let cookieCfg = defaultCookieSettings
            let ticklerEnv =
                    TicklerServerEnv
                    { envConnectionPool = pool
                    , envCookieSettings = cookieCfg
                    , envJWTSettings = jwtCfg
                    , envAdmins = serveSetAdmins
                    }
            liftIO $ Warp.run serveSetPort $ ticklerApp ticklerEnv

runLoopers_ :: ServeSettings -> IO ()
runLoopers_ ServeSettings {..} = do
    runLoopers serveSetLooperSettings

ticklerApp :: TicklerServerEnv -> Wai.Application
ticklerApp se =
    addPolicy . serveWithContext ticklerAPI (ticklerAppContext se) $
    makeTicklerServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
        simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        , corsMethods = ["GET", "POST", "HEAD", "DELETE"]
        }

makeTicklerServer :: TicklerServerEnv -> Server TicklerAPI
makeTicklerServer cfg =
    hoistServerWithContext
        ticklerAPI
        (Proxy :: Proxy TicklerContext)
        (`runReaderT` cfg)
        (toServant ticklerServer)

ticklerAppContext :: TicklerServerEnv -> Context TicklerContext
ticklerAppContext TicklerServerEnv {..} =
    envCookieSettings :. envJWTSettings :. EmptyContext

type TicklerContext = '[ CookieSettings, JWTSettings]
