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
              DB.insert_ $ intrayTrigger {intrayTriggerUser = userIdentifier user}

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

    it "it does not schedule these items without a trigger" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \triggeredItemPrototypes -> do
          -- Set up a user without an intray trigger
          runPersistentTest pool $ DB.insert_ (user :: User)

          -- Make sure the triggered items have unique uuids and belong to the user
          triggeredItems <- forM triggeredItemPrototypes $ \ti -> do
            uuid <- nextRandomUUID
            pure
              $ ti
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

    it "does not schedule other users' tickles" $ \pool ->
      forAllValid $ \user1 ->
        forAllValid $ \user2 ->
          forAllValid $ \user1IntrayTrigger ->
            forAllValid $ \user2IntrayTrigger ->
              forAllValid $ \user1TriggeredItemPrototypes -> do
                forAllValid $ \user2TriggeredItemPrototypes -> do
                  -- Set up an intray trigger
                  runPersistentTest pool $ do
                    -- One user with a trigger and the other without
                    DB.insert_ (user1 :: User)
                    DB.insert_ (user2 :: User)
                    DB.insert_ $ user1IntrayTrigger {intrayTriggerUser = userIdentifier user1}
                    DB.insert_ $ user2IntrayTrigger {intrayTriggerUser = userIdentifier user2}

                  -- Make sure the triggered items have unique uuids and belong to the user
                  user1TriggeredItems <- forM user1TriggeredItemPrototypes $ \ti -> do
                    uuid <- nextRandomUUID
                    pure $ ti {triggeredItemIdentifier = uuid, triggeredItemUserId = userIdentifier user1}
                  user2TriggeredItems <- forM user2TriggeredItemPrototypes $ \ti -> do
                    uuid <- nextRandomUUID
                    pure $ ti {triggeredItemIdentifier = uuid, triggeredItemUserId = userIdentifier user2}

                  -- Set up the triggered items
                  runPersistentTest pool $ do
                    DB.insertMany_ (user1TriggeredItems :: [TriggeredItem])
                    DB.insertMany_ (user2TriggeredItems :: [TriggeredItem])

                  -- Run the looper
                  testRunLooper pool runTriggeredIntrayItemScheduler

                  -- Check that the intray items were scheduled to be sent to each their own user.
                  user1TriggeredIntrayItems <-
                    runPersistentTest pool
                      $ DB.selectList [TriggeredIntrayItemTrigger DB.==. intrayTriggerIdentifier user1IntrayTrigger] []
                  sort (map (triggeredIntrayItemItem . DB.entityVal) user1TriggeredIntrayItems)
                    `shouldBe` sort (map triggeredItemIdentifier user1TriggeredItems)
                  user2TriggeredIntrayItems <-
                    runPersistentTest pool
                      $ DB.selectList [TriggeredIntrayItemTrigger DB.==. intrayTriggerIdentifier user2IntrayTrigger] []
                  sort (map (triggeredIntrayItemItem . DB.entityVal) user2TriggeredIntrayItems)
                    `shouldBe` sort (map triggeredItemIdentifier user2TriggeredItems)

    it "is idempotent" $ \pool ->
      forAllValid $ \user1 ->
        forAllValid $ \user2 ->
          forAllValid $ \intrayTrigger ->
            forAllValid $ \user1TriggeredItemPrototypes -> do
              forAllValid $ \user2TriggeredItemPrototypes -> do
                -- Set up an intray trigger
                runPersistentTest pool $ do
                  -- One user with a trigger and the other without
                  DB.insert_ (user1 :: User)
                  DB.insert_ (user2 :: User)
                  DB.insert_ $ intrayTrigger {intrayTriggerUser = userIdentifier user1}

                -- Make sure the triggered items have unique uuids and belong to the user
                user1TriggeredItems <- forM user1TriggeredItemPrototypes $ \ti -> do
                  uuid <- nextRandomUUID
                  pure $ ti {triggeredItemIdentifier = uuid, triggeredItemUserId = userIdentifier user1}
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
                  runPersistentTest pool
                    $ DB.selectList [] []

                testRunLooper pool runTriggeredIntrayItemScheduler
                triggeredIntrayItemsAfterSecondRun <-
                  runPersistentTest pool
                    $ DB.selectList [] []

                triggeredIntrayItemsAfterSecondRun `shouldBe` (triggeredIntrayItemsAfterFirstRun :: [DB.Entity TriggeredIntrayItem])
