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

import Yesod.Auth
import Yesod.Test

import Data.Text (Text)

import Network.HTTP.Types

import Database.Persist.Sqlite (mkSqliteConnectionInfo)

import Servant.Client (BaseUrl(..), ClientEnv(..))

import Tickler.Data

import qualified Tickler.Server.TestUtils as API

import Tickler.Web.Server
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse.Types

import Tickler.Data.Gen ()

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

ticklerTestServeSettings :: IO ServeSettings
ticklerTestServeSettings = do
    let connInfo = mkSqliteConnectionInfo "test.db"
    pure
        ServeSettings
        { serveSetPort = 8000
        , serveSetPersistLogins = False
        , serveSetAPIPort = 8001
        , serveSetAPIConnectionInfo = connInfo
        , serveSetAPIConnectionCount = 4
        , serveSetAPIAdmins = [fromJust $ parseUsername "admin"]
        }

ticklerWebServerSpec :: YesodSpec App -> Spec
ticklerWebServerSpec = b . a
  where
    a :: YesodSpec App -> SpecWith ClientEnv
    a =
        yesodSpecWithSiteGeneratorAndArgument
            (\(ClientEnv _ burl _) -> do
                 sets <- ticklerTestServeSettings
                 let sets' = sets {serveSetAPIPort = baseUrlPort burl}
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
    withExampleAccount $ \un p -> do
        loginTo un p
        func un p

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
    withAdminAccount $ \un p -> do
        loginTo un p
        func un p

withAdminAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withAdminAccountAndLogin_ = withAdminAccountAndLogin . const . const
