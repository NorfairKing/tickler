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

import Conduit
import Control.Monad.Logger
import Data.Cache
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Database.Persist
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
runTicklerServer settings@Settings {..} =
  runStderrLoggingT $
    filterLogger (\_ ll -> ll >= setLogLevel) $
      withSqlitePoolInfo (mkSqliteConnectionInfo (T.pack (fromAbsFile setDb))) 1 $
        \pool -> do
          runResourceT (runSqlPool (runMigration serverAutoMigration >> customMigrations) pool)
            `catch` ( \pe ->
                        liftIO $
                          die $
                            unlines
                              [ case pe of
                                  PersistError t -> T.unpack t
                                  _ -> show (pe :: PersistException),
                                unwords ["sqlite3", show setDb]
                              ]
                    )
          signingKey <- liftIO loadSigningKey
          let jwtCfg = defaultJWTSettings signingKey
          let cookieCfg = defaultCookieSettings
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
                    envMonetisation = mMonetisationEnv
                  }
          let serverThread = liftIO $ Warp.run setPort $ ticklerApp ticklerEnv
          let looperThread = runTicklerLoopers pool settings
          concurrently_ looperThread serverThread

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

customMigrations :: SqlPersistT (ResourceT (LoggingT IO)) ()
customMigrations = do
  userTriggerEntities <- selectList [] [Asc UserTriggerId]
  liftIO $ mapM_ print userTriggerEntities
  runConduit $
    selectSource [EmailTriggerUser ==. Nothing] []
      .| C.mapM_ migrateEmailTrigger
  runConduit $
    selectSource [IntrayTriggerUser ==. Nothing] []
      .| C.mapM_ migrateIntrayTrigger

migrateEmailTrigger :: Entity EmailTrigger -> SqlPersistT (ResourceT (LoggingT IO)) ()
migrateEmailTrigger (Entity etid et@EmailTrigger {..}) = do
  mUserTrigger <-
    selectFirst
      [ UserTriggerTriggerType ==. EmailTriggerType,
        UserTriggerTriggerId ==. emailTriggerIdentifier
      ]
      []
  case mUserTrigger of
    Nothing ->
      liftIO $
        die $
          unlines
            [ unwords ["Migration of email trigger failed:", show etid],
              "No UserTrigger found that matches it.",
              ppShow et
            ]
    Just (Entity _ UserTrigger {..}) -> update etid [EmailTriggerUser =. Just userTriggerUserId]

migrateIntrayTrigger :: Entity IntrayTrigger -> SqlPersistT (ResourceT (LoggingT IO)) ()
migrateIntrayTrigger (Entity itid it@IntrayTrigger {..}) = do
  mUserTrigger <-
    selectFirst
      [ UserTriggerTriggerType ==. IntrayTriggerType,
        UserTriggerTriggerId ==. intrayTriggerIdentifier
      ]
      []
  case mUserTrigger of
    Nothing ->
      liftIO $
        die $
          unlines
            [ unwords ["Migration of Intray trigger failed:", show itid],
              "No UserTrigger found that matches it.",
              ppShow it
            ]
    Just (Entity _ UserTrigger {..}) -> update itid [IntrayTriggerUser =. Just userTriggerUserId]
