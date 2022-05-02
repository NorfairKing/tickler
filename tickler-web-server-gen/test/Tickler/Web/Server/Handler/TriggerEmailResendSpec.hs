{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TriggerEmailResendSpec where

import qualified Database.Persist as DB
import Test.Syd.Persistent
import Test.Syd.Yesod
import TestImport
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerAndDatabaseSpec $ do
    it "can ask to resend the verification email" $ \(pool, yc) -> do
      forAllValid $ \emailAddress ->
        runYesodClientM yc $
          withExampleAccountAndLogin_ $ do
            get TriggersR
            statusIs 200
            request $ do
              setMethod methodPost
              setUrl TriggerAddEmailR
              addTokenFromCookie
              addPostParam "email-address" $ emailAddressText emailAddress
            statusIs 303
            locationShouldBe TriggersR
            _ <- followRedirect
            statusIs 200
            -- Find the email trigger to figure out its uuid
            mUuid <- liftIO $ runPersistentTest pool $ fmap (emailTriggerIdentifier . DB.entityVal) <$> DB.selectFirst [] []
            case mUuid of
              Nothing -> liftIO $ expectationFailure "Expected to find an email trigger"
              Just uuid -> do
                -- First delete any emails that we've already scheduled.
                liftIO $ runPersistentTest pool $ DB.deleteWhere [VerificationEmailTrigger DB.==. uuid]
                -- Ask to resend the verification email
                request $ do
                  setMethod methodPost
                  setUrl $ TriggerEmailResendR uuid
                  addTokenFromCookie
                statusIs 303
                locationShouldBe TriggersR
                _ <- followRedirect
                statusIs 200
                -- Find the verification email
                mVerificationEmail <- liftIO $ runPersistentTest pool $ fmap DB.entityVal <$> DB.selectFirst [VerificationEmailTrigger DB.==. uuid] []
                case mVerificationEmail of
                  Nothing -> liftIO $ expectationFailure "Expected to find an email trigger"
                  Just _ -> pure ()
