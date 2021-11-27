{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PostAddEmailTriggerSpec
  ( spec,
  )
where

import TestImport
import Tickler.API
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $
    describe "GetTrigger and PostAddEmailTrigger" $
      it "gets the trigger that was just added" $
        \cenv ->
          forAllValid $ \t ->
            withValidNewUser cenv $ \token -> do
              (uuid, ti) <-
                runClientOrError cenv $ do
                  uuid <- clientPostAddEmailTrigger token t
                  ti <- clientGetTrigger token uuid
                  pure (uuid, ti)
              triggerInfoIdentifier ti `shouldBe` uuid
