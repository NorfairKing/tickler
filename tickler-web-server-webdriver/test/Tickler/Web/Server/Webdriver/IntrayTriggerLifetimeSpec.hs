{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.IntrayTriggerLifetimeSpec (spec) where

import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Persist.Sql as DB
import qualified Intray.Client as Intray
import Intray.Server.TestUtils (intrayTestClientEnvSetupFunc)
import qualified Intray.Server.TestUtils as Intray
import Test.Syd.Webdriver.Yesod
import Tickler.Web.Server.Webdriver.TestImport

-- This entire mess is only necessary because we did the sydtest-discover trick to only run one selenium server.
-- And we need access to the database in order to get the verification email out.
spec :: WebdriverSpec App
spec = ticklerWebdriverWithDBSpec $
  it "Can go through the entire intray trigger lifetime without trouble" $ \(pool, wte) ->
    forAllValid $ \accessKeyName ->
      let man = appHTTPManager $ webdriverTestEnvApp wte
       in unSetupFunc (intrayTestClientEnvSetupFunc Nothing man) $ \ienv ->
            Intray.withValidNewUserAndData ienv $ \intrayUsername _ itoken -> do
              -- Add an intray access key that only permits adding items
              accessKeyCreated <-
                Intray.runClientOrError ienv $
                  Intray.clientPostAddAccessKey
                    itoken
                    Intray.AddAccessKey
                      { Intray.addAccessKeyName = accessKeyName,
                        Intray.addAccessKeyPermissions = S.singleton Intray.PermitAdd
                      }

              runWebdriverTestM wte $ do
                let runSqlHere :: SqlPersistT IO a -> WebdriverTestM App a
                    runSqlHere query = liftIO $ runSqlPool query pool

                let user = dummyUser
                -- Go to the homepage
                openRoute HomeR

                -- Create an account
                driveRegister user

                -- Navigate to the Account page
                openRoute AccountR

                -- Add an email trigger
                openRoute TriggersR
                let intrayBaseUrl = baseUrl ienv
                iti <-
                  driveTriggerAddIntray
                    user
                    intrayUsername
                    (baseUrl ienv)
                    (Intray.accessKeyCreatedKey accessKeyCreated)
                liftIO $ shouldBeValid iti

                -- Find the email trigger in the db to know its UUID
                mIntrayTrigger <- runSqlHere $ selectFirst [IntrayTriggerUrl ==. intrayBaseUrl] []
                triggerUUID <- case mIntrayTrigger of
                  Nothing -> liftIO $ expectationFailure "expected to have an intray trigger by now."
                  Just (Entity _ IntrayTrigger {..}) -> pure intrayTriggerIdentifier

                -- Check that the intray trigger is there
                openRoute TriggersR
                contents <- findElem (ById $ uuidText triggerUUID) >>= getText
                liftIO $ contents `shouldSatisfy` (T.pack (showBaseUrl intrayBaseUrl) `T.isInfixOf`)

                -- Delete the intray trigger
                openRoute TriggersR
                findElem (ById $ uuidText triggerUUID) >>= \e ->
                  findElemFrom e (ById "delete") >>= click
                acceptAlert
                getCurrentRoute >>= (liftIO . (`shouldBe` TriggersR))
