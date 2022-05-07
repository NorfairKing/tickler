{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Server.Looper.TriggeredIntrayItemSchedulerSpec (spec) where

import Data.List
import qualified Database.Persist as DB
import Test.Syd.Persistent
import TestImport
import Tickler.API
import Tickler.Data.Gen ()
import Tickler.Server.Looper.TriggeredIntrayItemScheduler
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerDatabase $ do
  describe "runTriggeredIntrayItemScheduler" $ do
    it "it schedules these items" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \intrayTrigger ->
          forAllValid $ \triggeredItemPrototypes -> do
            -- Set up an intray trigger
            runPersistentTest pool $ do
              DB.insert_ (user :: User)
              DB.insert_ (intrayTrigger :: IntrayTrigger)
              DB.insert_
                UserTrigger
                  { userTriggerUserId = userIdentifier user,
                    userTriggerTriggerType = IntrayTriggerType,
                    userTriggerTriggerId = intrayTriggerIdentifier intrayTrigger
                  }

            -- Make sure the triggered items have unique uuids and belong to the user
            triggeredItems <- forM triggeredItemPrototypes $ \ti -> do
              uuid <- nextRandomUUID
              pure $ ti {triggeredItemIdentifier = uuid, triggeredItemUserId = userIdentifier user}

            -- Set up the triggered items
            runPersistentTest pool $ DB.insertMany_ (triggeredItems :: [TriggeredItem])

            -- Run the looper
            testRunLooper pool runTriggeredIntrayItemScheduler

            -- Check that the intray items were scheduled to be sent.
            triggeredIntrayItems <- runPersistentTest pool $ DB.selectList [TriggeredIntrayItemTrigger DB.==. intrayTriggerIdentifier intrayTrigger] []
            sort (map (triggeredIntrayItemItem . DB.entityVal) triggeredIntrayItems) `shouldBe` sort (map triggeredItemIdentifier triggeredItems)

    it "it does not schedule these items" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \triggeredItemPrototypes -> do
          -- Set up a user without an intray trigger
          runPersistentTest pool $ DB.insert_ (user :: User)

          -- Make sure the triggered items have unique uuids and belong to the user
          triggeredItems <- forM triggeredItemPrototypes $ \ti -> do
            uuid <- nextRandomUUID
            pure $
              ti
                { triggeredItemIdentifier = uuid,
                  triggeredItemUserId = userIdentifier user
                }

          -- Set up the triggered items
          runPersistentTest pool $ DB.insertMany_ (triggeredItems :: [TriggeredItem])

          -- Run the looper
          testRunLooper pool runTriggeredIntrayItemScheduler

          -- Check that the intray items were scheduled to be sent.
          triggeredIntrayItems <- runPersistentTest pool $ DB.selectList [] []
          map (triggeredIntrayItemItem . DB.entityVal) triggeredIntrayItems `shouldBe` []

    it "is idempotent" $ \pool ->
      forAllValid $ \user1 ->
        forAllValid $ \user2 ->
          forAllValid $ \intrayTrigger ->
            forAllValid $ \user1TriggeredItemPrototypes -> do
              forAllValid $ \user2TriggeredItemPrototypes -> do
                -- Set up an intray trigger
                runPersistentTest pool $ do
                  DB.insert_ (user1 :: User)
                  DB.insert_ (user2 :: User)
                  DB.insert_ (intrayTrigger :: IntrayTrigger)
                  DB.insert_
                    UserTrigger
                      { userTriggerUserId = userIdentifier user1,
                        userTriggerTriggerType = IntrayTriggerType,
                        userTriggerTriggerId = intrayTriggerIdentifier intrayTrigger
                      }

                -- Make sure the triggered items have unique uuids and belong to the user
                user1TriggeredItems <- forM user1TriggeredItemPrototypes $ \ti -> do
                  uuid <- nextRandomUUID
                  pure $ ti {triggeredItemIdentifier = uuid, triggeredItemUserId = userIdentifier user2}
                user2TriggeredItems <- forM user2TriggeredItemPrototypes $ \ti -> do
                  uuid <- nextRandomUUID
                  pure $ ti {triggeredItemIdentifier = uuid, triggeredItemUserId = userIdentifier user2}

                -- Set up the triggered items
                runPersistentTest pool $ do
                  DB.insertMany_ (user1TriggeredItems :: [TriggeredItem])
                  DB.insertMany_ (user2TriggeredItems :: [TriggeredItem])

                -- Run the looper
                testRunLooper pool runTriggeredIntrayItemScheduler

                triggeredIntrayItemsAfterFirstRun <-
                  runPersistentTest pool $
                    DB.selectList [] []
                triggeredIntrayItemsAfterSecondRun <-
                  runPersistentTest pool $
                    DB.selectList [] []
                triggeredIntrayItemsAfterSecondRun `shouldBe` (triggeredIntrayItemsAfterFirstRun :: [DB.Entity TriggeredIntrayItem])

  describe "scheduleTriggeredIntrayItem" $ do
    it "it schedules this item" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \intrayTrigger ->
          forAllValid $ \triggeredItemPrototype -> do
            runPersistentTest pool $ do
              -- Set up an intray trigger
              DB.insert_ (user :: User)
              DB.insert_ (intrayTrigger :: IntrayTrigger)
              DB.insert_
                UserTrigger
                  { userTriggerUserId = userIdentifier user,
                    userTriggerTriggerType = IntrayTriggerType,
                    userTriggerTriggerId = intrayTriggerIdentifier intrayTrigger
                  }

            -- Make sure the triggered item has a unique uuid and belongs to the user
            uuid <- nextRandomUUID
            let triggeredItem =
                  triggeredItemPrototype
                    { triggeredItemIdentifier = uuid,
                      triggeredItemUserId = userIdentifier user
                    }

            -- Set up the triggered item
            triggeredItemId <- runPersistentTest pool $ DB.insert (triggeredItem :: TriggeredItem)

            -- Run the looper
            testRunLooper pool $ scheduleTriggeredIntrayItem $ DB.Entity triggeredItemId triggeredItem

            -- Check that the intray items were scheduled to be sent.
            mTriggeredIntrayItem <-
              runPersistentTest pool $
                DB.selectFirst [TriggeredIntrayItemTrigger DB.==. intrayTriggerIdentifier intrayTrigger] []
            case mTriggeredIntrayItem of
              Nothing -> expectationFailure "Should have found a triggered intray item."
              Just (DB.Entity _ TriggeredIntrayItem {..}) -> triggeredIntrayItemItem `shouldBe` triggeredItemIdentifier triggeredItem

    it "it does not schedule this item" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \triggeredItemPrototype -> do
          -- Set up an intray trigger
          runPersistentTest pool $ DB.insert_ (user :: User)

          -- Make sure the triggered item has a unique uuid and belongs to the user
          uuid <- nextRandomUUID
          let triggeredItem =
                triggeredItemPrototype
                  { triggeredItemIdentifier = uuid,
                    triggeredItemUserId = userIdentifier user
                  }

          -- Set up the triggered item
          triggeredItemId <- runPersistentTest pool $ DB.insert (triggeredItem :: TriggeredItem)

          -- Run the looper
          testRunLooper pool $ scheduleTriggeredIntrayItem $ DB.Entity triggeredItemId triggeredItem

          -- Check that the intray items were scheduled to be sent.
          mTriggeredIntrayItem <- runPersistentTest pool $ DB.selectFirst [] []
          case mTriggeredIntrayItem of
            Nothing -> pure ()
            Just (_ :: DB.Entity TriggeredIntrayItem) -> expectationFailure "Should not have found a triggered intray item."
