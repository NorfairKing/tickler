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

import Tickler.Server.SigningKey

import Tickler.Server.Handler (ticklerServer)

runTicklerServer :: ServeSettings -> IO ()
runTicklerServer ServeSettings {..} =
    runStderrLoggingT $
    withSqlitePoolInfo serveSetConnectionInfo serveSetConnectionCount $ \pool -> do
        runResourceT $ flip runSqlPool pool $ runMigration migrateAll
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
