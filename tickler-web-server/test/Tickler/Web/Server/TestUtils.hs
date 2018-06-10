{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.TestUtils
    ( ticklerTestServeSettings
    , ticklerWebServerSpec
    , withExampleAccount
    , withExampleAccount_
    , withExampleAccountAndLogin
    , withExampleAccountAndLogin_
    , withAdminAccount
    , withAdminAccount_
    , withAdminAccountAndLogin
    , withAdminAccountAndLogin_
    ) where

import TestImport

import Control.Lens
import Data.Text (Text)

import Yesod.Auth
import Yesod.Test

import Network.HTTP.Types

import Database.Persist.Sqlite (mkSqliteConnectionInfo, walEnabled)

import Servant.Client (BaseUrl(..), ClientEnv(..))

import Tickler.Data

import qualified Tickler.Server.OptParse.Types as API
import qualified Tickler.Server.TestUtils as API

import Tickler.Web.Server
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse.Types

import Tickler.Data.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

ticklerTestServeSettings :: IO ServeSettings
ticklerTestServeSettings = do
    let connInfo =
            mkSqliteConnectionInfo "tickler-test.db" & walEnabled .~ False
    pure
        ServeSettings
        { serveSetPort = 8000
        , serveSetPersistLogins = False
        , serveSetDefaultIntrayUrl = Nothing
        , serveSetAPISettings =
              API.ServeSettings
              { API.serveSetPort = 8001
              , API.serveSetConnectionInfo = connInfo
              , API.serveSetConnectionCount = 1
              , API.serveSetAdmins = catMaybes [parseUsername "admin"]
              , API.serveSetLooperSettings =
                    API.LooperSettings
                    { API.looperSetConnectionInfo = connInfo
                    , API.looperSetConnectionCount = 1
                    , API.looperSetTriggererSets = API.LooperDisabled
                    , API.looperSetEmailerSets = API.LooperDisabled
                    , API.looperSetTriggeredIntrayItemSchedulerSets =
                          API.LooperDisabled
                    , API.looperSetTriggeredIntrayItemSenderSets =
                          API.LooperDisabled
                    , API.looperSetVerificationEmailConverterSets =
                          API.LooperDisabled
                    , API.looperSetTriggeredEmailSchedulerSets =
                          API.LooperDisabled
                    , API.looperSetTriggeredEmailConverterSets =
                          API.LooperDisabled
                    }
              }
        }

ticklerWebServerSpec :: YesodSpec App -> Spec
ticklerWebServerSpec = b . a
  where
    a :: YesodSpec App -> SpecWith ClientEnv
    a =
        yesodSpecWithSiteGeneratorAndArgument
            (\(ClientEnv _ burl _) -> do
                 sets_ <- ticklerTestServeSettings
                 let apiSets =
                         (serveSetAPISettings sets_)
                         {API.serveSetPort = baseUrlPort burl}
                 let sets' = sets_ {serveSetAPISettings = apiSets}
                 makeTicklerApp sets')
    b :: SpecWith ClientEnv -> Spec
    b = API.withTicklerServer

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
       Username
    -> Text
    -> (Username -> Text -> YesodExample App a)
    -> YesodExample App a
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

withExampleAccount ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccount =
    withFreshAccount (fromJust $ parseUsername "example") "pass"

withExampleAccountAndLogin ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccountAndLogin func =
    withExampleAccount $ \un_ p -> do
        loginTo un_ p
        func un_ p

withExampleAccount_ :: YesodExample App a -> YesodExample App a
withExampleAccount_ = withExampleAccount . const . const

withExampleAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withExampleAccountAndLogin_ = withExampleAccountAndLogin . const . const

withAdminAccount ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccount = withFreshAccount (fromJust $ parseUsername "admin") "admin"

withAdminAccount_ :: YesodExample App a -> YesodExample App a
withAdminAccount_ = withAdminAccount . const . const

withAdminAccountAndLogin ::
       (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccountAndLogin func =
    withAdminAccount $ \un_ p -> do
        loginTo un_ p
        func un_ p

withAdminAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withAdminAccountAndLogin_ = withAdminAccountAndLogin . const . const
