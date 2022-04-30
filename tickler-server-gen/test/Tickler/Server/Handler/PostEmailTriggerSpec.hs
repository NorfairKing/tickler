{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
      forAllValid $ \t ->
        withValidNewUser cenv $ \token -> do
          (uuid, ti) <-
            runClientOrError cenv $ do
              uuid <- clientPostEmailTrigger token t
              ti <- clientGetTrigger token uuid
              pure (uuid, ti)
          triggerInfoIdentifier ti `shouldBe` uuid
