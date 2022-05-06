{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggererSpec (spec) where

import Data.Time
import qualified Database.Persist as DB
import Test.Syd.Persistent
import TestImport
import Tickler.API
import Tickler.Client
import Tickler.Data.Gen ()
import Tickler.Server.Looper.Triggerer
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  describe "shouldBeTriggered" $ it "doesn't crash" $ producesValid3 shouldBeTriggered

  describe "ticklerItemLocalScheduledTime" $
    it "makes valid local times" $
      producesValid ticklerItemLocalScheduledTime

  describe "makeTriggeredItem" $
    it "produces valid ticklerItems when it succeeds" $
      producesValid3 makeTriggeredItem

  withTicklerServerAndDatabase $ do
    describe "considerTicklerItem" $
      it "triggers this item I just made for the past" $ \(pool, cenv) ->
        forAllValid $ \ticklePrototype ->
          withValidNewUser cenv $ \token -> do
            let day = fromGregorian 2022 05 04
            let tickle = ticklePrototype {tickleScheduledDay = day}

            -- Set up a tickle scheduled in the past
            uuid <- runClientOrError cenv $ clientPostItem token tickle
            mTicklerItem <- runPersistentTest pool $ DB.selectFirst [TicklerItemIdentifier DB.==. uuid] []
            ticklerItemEntity <- case mTicklerItem of
              Nothing -> expectationFailure "Expected the item to be there."
              Just ticklerItemEntity -> pure ticklerItemEntity

            -- Run the looper
            testRunLooper pool $ considerTicklerItem ticklerItemEntity

            -- Check that the item was triggered
            mTriggeredItem <- runPersistentTest pool $ DB.selectFirst [TriggeredItemScheduledDay DB.==. day] []
            case mTriggeredItem of
              Nothing -> expectationFailure "The item should have been triggered."
              Just (DB.Entity _ TriggeredItem {..}) -> triggeredItemContents `shouldBe` tickleContent tickle

    describe "runTriggerer" $
      it "triggers this item I just made for the past" $ \(pool, cenv) ->
        forAllValid $ \ticklePrototype ->
          withValidNewUser cenv $ \token -> do
            let day = fromGregorian 2022 05 05
            let tickle = ticklePrototype {tickleScheduledDay = day}

            -- Set up a tickle scheduled in the past
            _ <- runClientOrError cenv $ clientPostItem token tickle

            -- Run the looper
            testRunLooper pool runTriggerer

            -- Check that the item was triggered
            mTriggeredItem <- runPersistentTest pool $ DB.selectFirst [TriggeredItemScheduledDay DB.==. day] []
            case mTriggeredItem of
              Nothing -> expectationFailure "The item should have been triggered."
              Just (DB.Entity _ TriggeredItem {..}) -> triggeredItemContents `shouldBe` tickleContent tickle
