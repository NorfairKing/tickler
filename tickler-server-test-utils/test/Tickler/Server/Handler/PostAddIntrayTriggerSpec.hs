{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.PostAddIntrayTriggerSpec
    ( spec
    ) where

import TestImport

import Tickler.API
import Tickler.Client

import Tickler.Client.Gen ()
import Tickler.Data.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
    withTicklerServer $
    describe "GetTrigger and PostAddIntrayTrigger" $
    it "gets the trigger that was just added" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \token -> do
                (uuid, ti) <-
                    runClientOrError cenv $ do
                        uuid <- clientPostAddIntrayTrigger token t
                        ti <- clientGetTrigger token uuid
                        pure (uuid, ti)
                triggerInfoIdentifier ti `shouldBe` uuid
