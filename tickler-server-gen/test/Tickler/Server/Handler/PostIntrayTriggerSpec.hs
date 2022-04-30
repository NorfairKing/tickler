{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Tickler.Server.Handler.PostIntrayTriggerSpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Intray.Client as Intray
import qualified Intray.Server.TestUtils as Intray
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  withTicklerServer $
    it "fails to add an intray trigger if the intray server is down" $ \tenv ->
      forAllValid $ \intrayUsername ->
        forAllValid $ \intrayAccessKey ->
          withValidNewUser tenv $ \ttoken -> do
            intrayBaseUrl <- parseBaseUrl "intray.example.com"
            errOrUuid <-
              runClientOrError tenv $
                clientPostIntrayTrigger
                  ttoken
                  AddIntrayTrigger
                    { addIntrayTriggerUrl = intrayBaseUrl,
                      addIntrayTriggerUsername = intrayUsername,
                      addIntrayTriggerAccessKey = intrayAccessKey
                    }
            case errOrUuid of
              Right _ -> expectationFailure "should not have succeeded."
              Left _ -> pure ()

  withBothTicklerAndIntrayServer $
    it "gets the trigger that was just added" $ \(tenv, ienv) ->
      forAllValid $ \name ->
        withValidNewUser tenv $ \ttoken ->
          Intray.withValidNewUserAndData ienv $ \un _ itoken -> do
            -- Add an intray access key that only permits adding items
            akc <-
              runClientOrError ienv $
                Intray.clientPostAddAccessKey
                  itoken
                  Intray.AddAccessKey
                    { Intray.addAccessKeyName = name,
                      Intray.addAccessKeyPermissions = S.singleton Intray.PermitAdd
                    }
            runClientOrError tenv $ do
              errOrUuid <-
                clientPostIntrayTrigger
                  ttoken
                  AddIntrayTrigger
                    { addIntrayTriggerUrl = baseUrl ienv,
                      addIntrayTriggerUsername = un,
                      addIntrayTriggerAccessKey = Intray.accessKeyCreatedKey akc
                    }
              case errOrUuid of
                Left err -> liftIO $ expectationFailure (T.unpack err)
                Right uuid -> do
                  TriggerInfo {..} <- clientGetTrigger ttoken uuid
                  liftIO $ do
                    let TriggerInfo _ _ = undefined
                    triggerInfoIdentifier `shouldBe` uuid
                    case triggerInfo of
                      TriggerEmail _ -> expectationFailure "should have been an intray trigger."
                      TriggerIntray IntrayTriggerInfo {..} -> do
                        let IntrayTriggerInfo _ = undefined
                        intrayTriggerInfoUrl `shouldBe` baseUrl ienv
