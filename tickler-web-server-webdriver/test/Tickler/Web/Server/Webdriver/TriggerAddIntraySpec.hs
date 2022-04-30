{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Web.Server.Webdriver.TriggerAddIntraySpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Intray.Client as Intray
import Intray.Server.TestUtils (intrayTestClientEnvSetupFunc)
import qualified Intray.Server.TestUtils as Intray
import Network.HTTP.Client as HTTP
import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec =
  itWithAll "can add an intray trigger" $ \(HCons _ (HCons man HNil) :: HList '[SeleniumServerHandle, HTTP.Manager]) wte ->
    forAllValid $ \accessKeyName ->
      unSetupFunc (intrayTestClientEnvSetupFunc Nothing man) $ \ienv ->
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
              findElem (ById "nav-triggers") >>= click
              -- Url
              urlE <- findElem (ByName "url")
              clearInput urlE
              sendKeys (T.pack (showBaseUrl (baseUrl ienv))) urlE
              -- Username
              usernameE <- findElem (ByName "username")
              clearInput usernameE
              sendKeys (Intray.usernameText intrayUsername) usernameE
              -- AccessKey
              findElem (ByName "access-key") >>= sendKeys (Intray.accessKeySecretText (Intray.accessKeyCreatedKey accessKeyCreated))
              findElem (ById "submit-intray") >>= submit
              -- Check that the trigger exists now.
              token <- getUserToken $ testUserUsername dummyUser
              triggers <- driveClientOrErr $ clientGetTriggers token
              liftIO $ case triggers of
                [] -> expectationFailure "Got no triggers."
                [TriggerInfo {..}] -> case triggerInfo of
                  TriggerIntray IntrayTriggerInfo {..} -> intrayTriggerInfoUrl `shouldBe` baseUrl ienv
                  _ -> expectationFailure "Expected an intray trigger."
                _ -> expectationFailure "Got more than one trigger."
