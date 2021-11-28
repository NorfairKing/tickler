{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.TestUtils
  ( ticklerTestServeSettings,
    ticklerWebServerSpec,
    withExampleAccount,
    withExampleAccount_,
    withExampleAccountAndLogin,
    withExampleAccountAndLogin_,
    withAdminAccount,
    withAdminAccount_,
    withAdminAccountAndLogin,
    withAdminAccountAndLogin_,
  )
where

import Control.Lens
import Control.Monad.Logger
import Database.Persist.Sqlite (mkSqliteConnectionInfo, walEnabled)
import qualified Network.HTTP.Client as HTTP
import Servant.Client (ClientEnv (..))
import Test.Syd.Yesod
import TestImport
import Tickler.Data
import Tickler.Data.Gen ()
import qualified Tickler.Server.OptParse.Types as API
import qualified Tickler.Server.TestUtils as API
import Tickler.Web.Server
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse.Types
import Yesod.Auth

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

ticklerTestServeSettings :: IO ServeSettings
ticklerTestServeSettings = do
  let connInfo = mkSqliteConnectionInfo "tickler-test.db" & walEnabled .~ False
  pure
    ServeSettings
      { serveSetPort = 8000,
        serveSetPersistLogins = False,
        serveSetDefaultIntrayUrl = Nothing,
        serveSetTracking = Nothing,
        serveSetVerification = Nothing,
        serveSetAPISettings =
          API.ServeSettings
            { API.serveSetPort = 8001,
              API.serveSetLogLevel = LevelWarn,
              API.serveSetConnectionInfo = connInfo,
              API.serveSetAdmins = catMaybes [parseUsername "admin"],
              API.serveSetFreeloaders = catMaybes [parseUsername "freeloader"],
              API.serveSetMonetisationSettings = Nothing,
              API.serveSetLoopersSettings =
                API.LoopersSettings
                  { API.looperSetTriggererSets = API.LooperDisabled,
                    API.looperSetEmailerSets = API.LooperDisabled,
                    API.looperSetTriggeredIntrayItemSchedulerSets = API.LooperDisabled,
                    API.looperSetTriggeredIntrayItemSenderSets = API.LooperDisabled,
                    API.looperSetVerificationEmailConverterSets = API.LooperDisabled,
                    API.looperSetTriggeredEmailSchedulerSets = API.LooperDisabled,
                    API.looperSetTriggeredEmailConverterSets = API.LooperDisabled,
                    API.looperSetAdminNotificationEmailConverterSets = API.LooperDisabled
                  }
            }
      }

ticklerWebServerSpec :: YesodSpec App -> Spec
ticklerWebServerSpec = b . a
  where
    a :: YesodSpec App -> TestDef '[Manager] ClientEnv
    a = yesodSpecWithSiteSetupFunc' appSetupFunc
    b :: TestDef '[Manager] ClientEnv -> Spec
    b = API.withTicklerServer

appSetupFunc :: HTTP.Manager -> ClientEnv -> SetupFunc App
appSetupFunc _ (ClientEnv _ burl _) = do
  sets_ <- liftIO ticklerTestServeSettings
  let apiSets = (serveSetAPISettings sets_) {API.serveSetPort = baseUrlPort burl}
  let sets' = sets_ {serveSetAPISettings = apiSets}
  liftIO $ makeTicklerApp sets'

loginTo :: Username -> Text -> YesodExample App ()
loginTo username passphrase = do
  get $ AuthR LoginR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR loginFormPostTargetR
    addTokenFromCookie
    addPostParam "userkey" $ usernameText username
    addPostParam "passphrase" passphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right AddR

withFreshAccount ::
  Username -> Text -> (Username -> Text -> YesodExample App a) -> YesodExample App a
withFreshAccount exampleUsername examplePassphrase func = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addTokenFromCookie
    addPostParam "username" $ usernameText exampleUsername
    addPostParam "passphrase" examplePassphrase
    addPostParam "passphrase-confirm" examplePassphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right AddR
  func exampleUsername examplePassphrase

withExampleAccount :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccount = withFreshAccount (fromJust $ parseUsername "example") "pass"

withExampleAccountAndLogin :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccountAndLogin func =
  withExampleAccount $ \un_ p -> do
    loginTo un_ p
    func un_ p

withExampleAccount_ :: YesodExample App a -> YesodExample App a
withExampleAccount_ = withExampleAccount . const . const

withExampleAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withExampleAccountAndLogin_ = withExampleAccountAndLogin . const . const

withAdminAccount :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccount = withFreshAccount (fromJust $ parseUsername "admin") "admin"

withAdminAccount_ :: YesodExample App a -> YesodExample App a
withAdminAccount_ = withAdminAccount . const . const

withAdminAccountAndLogin :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccountAndLogin func =
  withAdminAccount $ \un_ p -> do
    loginTo un_ p
    func un_ p

withAdminAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withAdminAccountAndLogin_ = withAdminAccountAndLogin . const . const
