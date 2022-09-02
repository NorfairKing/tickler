{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.OnboardingSpec (spec) where

import qualified Data.Text as T
import Database.Persist.Sql as DB
import Network.HTTP.Client as HTTP
import Test.Syd.Webdriver.Yesod
import Test.Syd.Yesod
import Tickler.Server.TestUtils as API
import Tickler.Web.Server.TestUtils as Web
import Tickler.Web.Server.Webdriver.TestImport

-- This entire mess is only necessary because we did the sydtest-discover trick to only run one selenium server.
-- And we need access to the database in order to get the verification email out.
spec :: WebdriverSpec App
spec = setupAroundWith go deepSpec
  where
    go :: WebdriverTestEnv App -> SetupFunc ()
    go _ = pure ()

deepSpec :: TestDef '[SeleniumServerHandle, HTTP.Manager] ()
deepSpec = setupAroundWith' go2 $ setupAroundWith' go1 withDBSpec
  where
    go2 :: SeleniumServerHandle -> () -> SetupFunc SeleniumServerHandle
    go2 ssh () = pure ssh

    go1 :: HTTP.Manager -> SeleniumServerHandle -> SetupFunc (DB.ConnectionPool, WebdriverTestEnv App)
    go1 man ssh = do
      (pool, cenv) <-
        API.ticklerTestClientEnvAndDatabaseSetupFunc
          Nothing -- No monetisation.
          man
      app <- Web.appSetupFunc man cenv
      yesodClient <- yesodClientSetupFunc man app
      wte <-
        webdriverTestEnvSetupFunc
          ssh
          man
          (yesodClientSiteURI yesodClient)
          app
      pure (pool, wte)

withDBSpec :: TestDef '[SeleniumServerHandle, HTTP.Manager] (DB.ConnectionPool, WebdriverTestEnv App)
withDBSpec = do
  it "Can go through the entire onboarding flow without trouble" $ \(pool, wte) -> runWebdriverTestM wte $ do
    let runSqlHere :: SqlPersistT IO a -> WebdriverTestM App a
        runSqlHere query = liftIO $ runSqlPool query pool

    let user = dummyUser
    -- Go to the homepage
    openRoute HomeR

    -- Create an account
    driveRegister user

    -- Navigate to the Account page
    openRoute AccountR

    -- Navigate to the Settings page
    -- Set the timezone to UTC+2
    openRoute AccountSettingsR
    findElem (ByName "timezone") >>= sendKeys "UTC+2"
    findElem (ById "submit") >>= submit

    let ea :: EmailAddress
        ea = "tickler@example.com"

    -- Add an email trigger
    openRoute TriggersR
    eti <- driveTriggerAddEmail (testUserUsername user) (emailAddressText ea)
    liftIO $ shouldBeValid eti

    -- Find the email trigger in the db to know its UUID
    mEmailTrigger <- runSqlHere $ selectFirst [EmailTriggerAddress ==. ea] []
    triggerUUID <- case mEmailTrigger of
      Nothing -> liftIO $ expectationFailure "expected to have an email trigger by now."
      Just (Entity _ EmailTrigger {..}) -> pure emailTriggerIdentifier

    -- Request resending the email trigger verification email
    openRoute TriggersR
    findElem (ById $ uuidText triggerUUID) >>= \e ->
      findElemFrom e (ById "resend") >>= click
    acceptAlert
    getCurrentRoute >>= (liftIO . (`shouldBe` TriggersR))
    notificationText <- findElem (ByClass "notification") >>= getText
    liftIO $ notificationText `shouldSatisfy` ("Verification email already scheduled." `T.isInfixOf`)

    -- Find the verification email in the database to simulate having received
    -- it.
    mVerificationEmail <- runSqlHere $ selectFirst [VerificationEmailTo ==. ea] []
    verificationKey <- case mVerificationEmail of
      Nothing -> liftIO $ expectationFailure "expected an email to have been scheduled."
      Just (Entity _ VerificationEmail {..}) -> pure verificationEmailKey

    -- Verify the email trigger
    openRoute (TriggerEmailVerifyR triggerUUID verificationKey)

    -- Check that the email trigger is now verified
    openRoute TriggersR
    contents <- findElem (ById $ uuidText triggerUUID) >>= getText
    liftIO $ contents `shouldSatisfy` ("Verified" `T.isInfixOf`)

    -- Add a tickle
    uuid <- driveAddTickle (testUserUsername user) dummyTickle
    liftIO $ shouldBeValid uuid
