{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server
  ( runTicklerServer
  , makeTicklerServer
  , ticklerAppContext
  ) where

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Cache
import Database.Persist.Sqlite
import Import
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Server.Generic
import Tickler.API
import Tickler.Server.Handler
import Tickler.Server.Looper
import Tickler.Server.OptParse.Types
import Tickler.Server.SigningKey
import Tickler.Server.Types

runTicklerServer :: ServeSettings -> IO ()
runTicklerServer ServeSettings {..} =
  runStderrLoggingT $
  withSqlitePoolInfo serveSetConnectionInfo 1 $ \pool -> do
    runResourceT $ flip runSqlPool pool $ runMigration migrateAll
    signingKey <- liftIO loadSigningKey
    let jwtCfg = defaultJWTSettings signingKey
    let cookieCfg = defaultCookieSettings
    loopersHandle <- liftIO $ startLoopers pool serveSetLoopersSettings serveSetMonetisationSettings
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
            , envFreeloaders = serveSetFreeloaders
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
    , postItem = withAuthResult servePostItem
    , getItem = withAuthResult serveGetItem
    , deleteItem = withAuthResult serveDeleteItem
    , postRetryTriggered = withAuthResult servePostRetryTriggered
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
    , postChangePassphrase = withAuthResult servePostChangePassphrase
    , putAccountSettings = withAuthResult servePutAccountSettings
    , deleteAccount = withAuthResult serveDeleteAccount
    }

ticklerPublicServer :: TicklerPublicSite (AsServerT TicklerHandler)
ticklerPublicServer =
  TicklerPublicSite
    { postRegister = servePostRegister
    , postLogin = servePostLogin
    , getLoopersStatus = serveGetLoopersStatus
    , getDocs = serveGetDocs
    , getPricing = serveGetPricing
    }

ticklerAdminServer :: TicklerAdminSite (AsServerT TicklerHandler)
ticklerAdminServer =
  TicklerAdminSite
    { adminGetStats = withAuthResult serveAdminGetStats
    , adminDeleteAccount = withAuthResult serveAdminDeleteAccount
    , adminGetAccounts = withAuthResult serveAdminGetAccounts
    }

withAuthResult :: ThrowAll a => (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401
