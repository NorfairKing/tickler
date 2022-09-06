{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.EmailTriggerLifetimeSpec (spec) where

import qualified Data.Text as T
import Database.Persist.Sql as DB
import Test.Syd.Webdriver.Yesod
import Tickler.Web.Server.Webdriver.TestImport

-- This entire mess is only necessary because we did the sydtest-discover trick to only run one selenium server.
-- And we need access to the database in order to get the verification email out.
spec :: WebdriverSpec App
spec = ticklerWebdriverWithDBSpec $
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

    -- Delete the email trigger
    openRoute TriggersR
    findElem (ById $ uuidText triggerUUID) >>= \e ->
      findElemFrom e (ById "delete") >>= click
    acceptAlert
    getCurrentRoute >>= (liftIO . (`shouldBe` TriggersR))
