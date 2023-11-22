{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Web.Server.Webdriver.TriggerAddIntraySpec (spec) where

import qualified Data.Set as S
import qualified Intray.Client as Intray
import Intray.Server.TestUtils (intrayTestClientEnvSetupFunc)
import qualified Intray.Server.TestUtils as Intray
import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec =
  it "can add an intray trigger" $ \wte ->
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

              runWebdriverTestM wte $
                driveAsNewUser dummyUser $ do
                  IntrayTriggerInfo {..} <-
                    driveTriggerAddIntray
                      dummyUser
                      intrayUsername
                      (baseUrl ienv)
                      (Intray.accessKeyCreatedKey accessKeyCreated)
                  liftIO $ intrayTriggerInfoUrl `shouldBe` baseUrl ienv
