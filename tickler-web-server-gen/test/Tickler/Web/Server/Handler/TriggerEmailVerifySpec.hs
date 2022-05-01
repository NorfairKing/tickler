{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Handler.TriggerEmailVerifySpec where

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
    it "can post an email trigger" $ \(pool, yc) -> do
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
            mVerificationEmail <- liftIO $ runPersistentTest pool $ fmap DB.entityVal <$> DB.selectFirst [] []
            case mVerificationEmail of
              Nothing -> liftIO $ expectationFailure "Expected to find an email trigger"
              Just VerificationEmail {..} -> do
                get $ TriggerEmailVerifyR verificationEmailTrigger verificationEmailKey
                statusIs 303
                locationShouldBe TriggersR
                _ <- followRedirect
                statusIs 200
