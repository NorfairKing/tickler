{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PostAddIntrayTriggerSpec
  ( spec
  ) where

import TestImport

import qualified Data.Set as S
import qualified Data.Text as T

import qualified Intray.Client as Intray
import qualified Intray.Server.TestUtils as Intray

import Tickler.Client

import Tickler.API.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withBothTicklerAndIntrayServer $
  describe "GetTrigger and PostAddIntrayTrigger" $
  it "gets the trigger that was just added" $ \(tenv, ienv) ->
    forAllValid $ \name ->
      withValidNewUser tenv $ \ttoken ->
        Intray.withValidNewUserAndData ienv $ \un _ itoken -> do
          akc <-
            runClientOrError ienv $
            Intray.clientPostAddAccessKey
              itoken
              Intray.AddAccessKey
                { Intray.addAccessKeyName = name
                , Intray.addAccessKeyPermissions = S.singleton Intray.PermitAdd
                }
          (uuid, ti) <-
            runClientOrError tenv $ do
              errOrUuid <-
                clientPostAddIntrayTrigger
                  ttoken
                  AddIntrayTrigger
                    { addIntrayTriggerUrl = baseUrl ienv
                    , addIntrayTriggerUsername = un
                    , addIntrayTriggerAccessKey = Intray.accessKeyCreatedKey akc
                    }
              case errOrUuid of
                Left err -> error (T.unpack err)
                Right uuid -> do
                  ti <- clientGetTrigger ttoken uuid
                  pure (uuid, ti)
          triggerInfoIdentifier ti `shouldBe` uuid
