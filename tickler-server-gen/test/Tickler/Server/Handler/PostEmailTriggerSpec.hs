{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Tickler.Server.Handler.PostEmailTriggerSpec (spec) where

import TestImport
import Tickler.API
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerServer $
  describe "GetTrigger and PostEmailTrigger" $
    it "gets the trigger that was just added" $ \cenv ->
      forAllValid $ \addEmailTrigger ->
        withValidNewUser cenv $ \token -> do
          runClientOrError cenv $ do
            uuid <- clientPostEmailTrigger token addEmailTrigger
            TriggerInfo {..} <- clientGetTrigger token uuid
            liftIO $ do
              triggerInfoIdentifier `shouldBe` uuid
              let TriggerInfo _ _ = undefined
              case triggerInfo of
                TriggerIntray _ -> expectationFailure "should have been an email trigger."
                TriggerEmail EmailTriggerInfo {..} -> do
                  emailTriggerInfoVerified `shouldBe` False
                  emailTriggerInfoEmailAddress `shouldBe` addEmailTriggerEmailAddress addEmailTrigger
