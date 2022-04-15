{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.TestUtils
  ( ticklerWebServerSpec,
    freeTicklerWebServerSpec,
    paidTicklerWebServerSpec,
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

import qualified Network.HTTP.Client as HTTP
import Servant.Client (ClientEnv (..))
import Test.Syd.Yesod
import TestImport
import Tickler.Data
import Tickler.Data.Gen ()
import qualified Tickler.Server.TestUtils as API
import Tickler.Web.Server
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse.Types
import Yesod.Auth

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

ticklerWebServerSpec :: YesodSpec App -> Spec
ticklerWebServerSpec = b . a
  where
    a :: YesodSpec App -> TestDef '[Manager] ClientEnv
    a = yesodSpecWithSiteSetupFunc' appSetupFunc
    b :: TestDef '[Manager] ClientEnv -> Spec
    b = API.withTicklerServer

freeTicklerWebServerSpec :: YesodSpec App -> Spec
freeTicklerWebServerSpec = b . a
  where
    a :: YesodSpec App -> TestDef '[Manager] ClientEnv
    a = yesodSpecWithSiteSetupFunc' appSetupFunc
    b :: TestDef '[Manager] ClientEnv -> Spec
    b = API.withFreeTicklerServer

paidTicklerWebServerSpec :: Int -> YesodSpec App -> Spec
paidTicklerWebServerSpec maxItems = b . a
  where
    a :: YesodSpec App -> TestDef '[Manager] ClientEnv
    a = yesodSpecWithSiteSetupFunc' appSetupFunc
    b :: TestDef '[Manager] ClientEnv -> Spec
    b = API.withPaidTicklerServer maxItems

appSetupFunc :: HTTP.Manager -> ClientEnv -> SetupFunc App
appSetupFunc _ (ClientEnv _ burl _) = do
  liftIO $
    makeTicklerApp
      Settings
        { setPort = 8000,
          setAPIBaseUrl = burl,
          setPersistLogins = False,
          setDefaultIntrayUrl = Nothing,
          setTracking = Nothing,
          setVerification = Nothing
        }

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
