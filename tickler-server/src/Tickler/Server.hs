{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server
  ( runTicklerServer,
    makeTicklerServer,
    ticklerAppContext,
  )
where

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Cache
import qualified Data.Text as T
import Database.Persist.Sqlite
import Import
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
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
import UnliftIO

runTicklerServer :: Settings -> IO ()
runTicklerServer Settings {..} =
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $
      withSqlitePoolInfo setConnectionInfo 1 $
        \pool -> do
          runResourceT (runSqlPool (runMigration migrateAll) pool)
            `catch` ( \pe -> liftIO $
                        die $ case pe of
                          PersistError t -> T.unpack t
                          _ -> show (pe :: PersistException)
                    )
          signingKey <- liftIO loadSigningKey
          let jwtCfg = defaultJWTSettings signingKey
          let cookieCfg = defaultCookieSettings
          loopersHandle <- startLoopers pool setLoopersSettings setMonetisationSettings
          mMonetisationEnv <-
            forM setMonetisationSettings $ \MonetisationSettings {..} -> do
              planCache <- liftIO $ newCache Nothing
              pure
                MonetisationEnv
                  { monetisationEnvStripeSettings = monetisationSetStripeSettings,
                    monetisationEnvMaxItemsFree = monetisationSetMaxItemsFree,
                    monetisationEnvPlanCache = planCache
                  }
          let ticklerEnv =
                TicklerServerEnv
                  { envConnectionPool = pool,
                    envCookieSettings = cookieCfg,
                    envJWTSettings = jwtCfg,
                    envAdmins = setAdmins,
                    envFreeloaders = setFreeloaders,
                    envMonetisation = mMonetisationEnv,
                    envLoopersHandle = loopersHandle
                  }
          liftIO $ Warp.run setPort $ ticklerApp ticklerEnv

ticklerApp :: TicklerServerEnv -> Wai.Application
ticklerApp se =
  addPolicy . serveWithContext ticklerAPI (ticklerAppContext se) $ makeTicklerServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"],
          corsMethods = ["GET", "POST", "HEAD", "DELETE"]
        }

makeTicklerServer :: TicklerServerEnv -> Server TicklerAPI
makeTicklerServer cfg =
  hoistServerWithContext
    ticklerAPI
    (Proxy :: Proxy TicklerContext)
    (`runReaderT` cfg)
    (genericServerT ticklerServer)

ticklerAppContext :: TicklerServerEnv -> Context TicklerContext
ticklerAppContext TicklerServerEnv {..} = envCookieSettings :. envJWTSettings :. EmptyContext

type TicklerContext = '[CookieSettings, JWTSettings]

ticklerServer :: TicklerSite (AsServerT TicklerHandler)
ticklerServer =
  TicklerSite
    { openSite = genericServerT ticklerOpenServer,
      adminSite = genericServerT ticklerAdminServer
    }

ticklerOpenServer :: TicklerOpenSite (AsServerT TicklerHandler)
ticklerOpenServer =
  TicklerOpenSite
    { protectedSite = genericServerT ticklerProtectedServer,
      publicSite = genericServerT ticklerPublicServer
    }

ticklerProtectedServer :: TicklerProtectedSite (AsServerT TicklerHandler)
ticklerProtectedServer =
  TicklerProtectedSite
    { getItems = withAuthResult serveGetItems,
      postItem = withAuthResult servePostItem,
      putItem = withAuthResult servePutItem,
      getItem = withAuthResult serveGetItem,
      deleteItem = withAuthResult serveDeleteItem,
      getTriggers = withAuthResult serveGetTriggers,
      getTrigger = withAuthResult serveGetTrigger,
      postIntrayTrigger = withAuthResult servePostIntrayTrigger,
      postEmailTrigger = withAuthResult servePostEmailTrigger,
      postEmailTriggerVerify = withAuthResult servePostEmailTriggerVerify,
      postEmailTriggerResendVerificationEmail =
        withAuthResult servePostEmailTriggerResendVerificationEmail,
      deleteTrigger = withAuthResult serveDeleteTrigger,
      getAccountInfo = withAuthResult serveGetAccountInfo,
      getAccountSettings = withAuthResult serveGetAccountSettings,
      postChangePassphrase = withAuthResult servePostChangePassphrase,
      putAccountSettings = withAuthResult servePutAccountSettings,
      deleteAccount = withAuthResult serveDeleteAccount
    }

ticklerPublicServer :: TicklerPublicSite (AsServerT TicklerHandler)
ticklerPublicServer =
  TicklerPublicSite
    { postRegister = servePostRegister,
      postLogin = servePostLogin,
      getPricing = serveGetPricing
    }

ticklerAdminServer :: TicklerAdminSite (AsServerT TicklerHandler)
ticklerAdminServer =
  TicklerAdminSite
    { adminGetStats = withAuthResult serveAdminGetStats,
      adminDeleteAccount = withAuthResult serveAdminDeleteAccount,
      adminGetAccounts = withAuthResult serveAdminGetAccounts
    }

withAuthResult :: ThrowAll a => (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401
