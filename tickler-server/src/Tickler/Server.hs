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

import Data.Cache

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Server.Generic

import Tickler.API

import Tickler.Server.OptParse.Types
import Tickler.Server.Types

import Tickler.Server.Handler
import Tickler.Server.Looper
import Tickler.Server.SigningKey

runTicklerServer :: ServeSettings -> IO ()
runTicklerServer ServeSettings {..} =
  runStderrLoggingT $
  withSqlitePoolInfo serveSetConnectionInfo 1 $ \pool -> do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    signingKey <- liftIO loadSigningKey
    let jwtCfg = defaultJWTSettings signingKey
    let cookieCfg = defaultCookieSettings
    loopersHandle <- liftIO $ startLoopers pool serveSetLooperSettings serveSetMonetisationSettings
    mMonetisationEnv <-
      forM serveSetMonetisationSettings $ \MonetisationSettings {..} -> do
        planCache <- liftIO $ newCache Nothing
        pure
          MonetisationEnv
            { monetisationEnvStripeSettings = monetisationSetStripeSettings
            , monetisationEnvMaxItemsFree = monetisationSetMaxItemsFree
            , monetisationEnvPlanCache = planCache
            }
    let ticklerEnv =
          TicklerServerEnv
            { envConnectionPool = pool
            , envCookieSettings = cookieCfg
            , envJWTSettings = jwtCfg
            , envAdmins = serveSetAdmins
            , envMonetisation = mMonetisationEnv
            , envLoopersHandle = loopersHandle
            }
    liftIO $ Warp.run serveSetPort $ ticklerApp ticklerEnv

ticklerApp :: TicklerServerEnv -> Wai.Application
ticklerApp se =
  addPolicy . serveWithContext ticklerAPI (ticklerAppContext se) $ makeTicklerServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
      simpleCorsResourcePolicy
        {corsRequestHeaders = ["content-type"], corsMethods = ["GET", "POST", "HEAD", "DELETE"]}

makeTicklerServer :: TicklerServerEnv -> Server TicklerAPI
makeTicklerServer cfg =
  hoistServerWithContext
    ticklerAPI
    (Proxy :: Proxy TicklerContext)
    (`runReaderT` cfg)
    (genericServerT ticklerServer)

ticklerAppContext :: TicklerServerEnv -> Context TicklerContext
ticklerAppContext TicklerServerEnv {..} = envCookieSettings :. envJWTSettings :. EmptyContext

type TicklerContext = '[ CookieSettings, JWTSettings]

ticklerServer :: TicklerSite (AsServerT TicklerHandler)
ticklerServer =
  TicklerSite
    {openSite = genericServerT ticklerOpenServer, adminSite = genericServerT ticklerAdminServer}

ticklerOpenServer :: TicklerOpenSite (AsServerT TicklerHandler)
ticklerOpenServer =
  TicklerOpenSite
    { protectedSite = genericServerT ticklerProtectedServer
    , publicSite = genericServerT ticklerPublicServer
    }

ticklerProtectedServer :: TicklerProtectedSite (AsServerT TicklerHandler)
ticklerProtectedServer =
  TicklerProtectedSite
    { getItemUUIDs = withAuthResult serveGetItemUUIDs
    , getItems = withAuthResult serveGetItems
    , postAddItem = withAuthResult servePostAddItem
    , getItem = withAuthResult serveGetItem
    , deleteItem = withAuthResult serveDeleteItem
    , retryTriggered = withAuthResult serveRetryTriggered
    , deleteTriggereds = withAuthResult serveDeleteTriggereds
    , postSync = withAuthResult servePostSync
    , getTriggers = withAuthResult serveGetTriggers
    , getTrigger = withAuthResult serveGetTrigger
    , postAddIntrayTrigger = withAuthResult servePostAddIntrayTrigger
    , postAddEmailTrigger = withAuthResult servePostAddEmailTrigger
    , postEmailTriggerVerify = withAuthResult servePostEmailTriggerVerify
    , postEmailTriggerResendVerificationEmail =
        withAuthResult servePostEmailTriggerResendVerificationEmail
    , deleteTrigger = withAuthResult serveDeleteTrigger
    , getAccountInfo = withAuthResult serveGetAccountInfo
    , getAccountSettings = withAuthResult serveGetAccountSettings
    , putAccountSettings = withAuthResult servePutAccountSettings
    , deleteAccount = withAuthResult serveDeleteAccount
    }

withAuthResult :: ThrowAll a => (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401
