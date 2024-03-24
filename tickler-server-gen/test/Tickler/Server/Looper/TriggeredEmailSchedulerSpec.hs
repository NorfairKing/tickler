{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Server.Looper.TriggeredEmailSchedulerSpec (spec) where

import Data.List
import qualified Database.Persist as DB
import Test.Syd.Persistent
import TestImport
import Tickler.API
import Tickler.Data.Gen ()
import Tickler.Server.Looper.TriggeredEmailScheduler
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerDatabase $ do
  describe "runTriggeredEmailScheduler" $ do
    it "it schedules these items" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \emailTrigger ->
          forAllValid $ \triggeredItemPrototypes -> do
            -- Set up an email trigger
            runPersistentTest pool $ do
              DB.insert_ (user :: User)
              DB.insert_
                $ emailTrigger
                  { emailTriggerUser = userIdentifier user,
                    emailTriggerVerified = True
                  }

            -- Make sure the triggered items have unique uuids and belong to the user
            triggeredItems <- forM triggeredItemPrototypes $ \ti -> do
              uuid <- nextRandomUUID
              pure $ ti {triggeredItemIdentifier = uuid, triggeredItemUserId = userIdentifier user}

            -- Set up the triggered items
            runPersistentTest pool $ DB.insertMany_ (triggeredItems :: [TriggeredItem])

            -- Run the looper
            testRunLooper pool runTriggeredEmailScheduler

            -- Check that the email items were scheduled to be sent.
            triggeredEmails <- runPersistentTest pool $ DB.selectList [TriggeredEmailTrigger DB.==. emailTriggerIdentifier emailTrigger] []
            sort (map (triggeredEmailItem . DB.entityVal) triggeredEmails) `shouldBe` sort (map triggeredItemIdentifier triggeredItems)

    it "it does not schedule these items because there is no trigger" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \triggeredItemPrototypes -> do
          -- Set up a user without an email trigger
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
          testRunLooper pool runTriggeredEmailScheduler

          -- Check that the email items were not scheduled to be sent.
          triggeredEmails <- runPersistentTest pool $ DB.selectList [] []
          map (triggeredEmailItem . DB.entityVal) triggeredEmails `shouldBe` []

    it "it does not schedule these items because the trigger is unverified" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \emailTrigger ->
          forAllValid $ \triggeredItemPrototypes -> do
            -- Set up a user without an email trigger
            runPersistentTest pool $ do
              DB.insert_ (user :: User)
              DB.insert_
                $ emailTrigger
                  { emailTriggerUser = userIdentifier user,
                    emailTriggerVerified = False
                  }

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
            testRunLooper pool runTriggeredEmailScheduler

            -- Check that the email items were not scheduled to be sent.
            triggeredEmails <- runPersistentTest pool $ DB.selectList [] []
            map (triggeredEmailItem . DB.entityVal) triggeredEmails `shouldBe` []

    it "does not schedule other users' tickles" $ \pool ->
      forAllValid $ \user1 ->
        forAllValid $ \user2 ->
          forAllValid $ \user1EmailTrigger ->
            forAllValid $ \user2EmailTrigger ->
              forAllValid $ \user1TriggeredItemPrototypes -> do
                forAllValid $ \user2TriggeredItemPrototypes -> do
                  -- Set up an email trigger
                  runPersistentTest pool $ do
                    -- One user with a trigger and the other without
                    DB.insert_ (user1 :: User)
                    DB.insert_ (user2 :: User)
                    DB.insert_
                      $ user1EmailTrigger
                        { emailTriggerUser = userIdentifier user1,
                          emailTriggerVerified = True
                        }
                    DB.insert_
                      $ user2EmailTrigger
                        { emailTriggerUser = userIdentifier user2,
                          emailTriggerVerified = True
                        }

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
                  testRunLooper pool runTriggeredEmailScheduler

                  -- Check that the email items were scheduled to be sent, each to their own user
                  user1TriggeredEmails <- runPersistentTest pool $ DB.selectList [TriggeredEmailTrigger DB.==. emailTriggerIdentifier user1EmailTrigger] []
                  sort (map (triggeredEmailItem . DB.entityVal) user1TriggeredEmails) `shouldBe` sort (map triggeredItemIdentifier user1TriggeredItems)

    it "is idempotent" $ \pool ->
      forAllValid $ \user1 ->
        forAllValid $ \user2 ->
          forAllValid $ \emailTrigger ->
            forAllValid $ \user1TriggeredItemPrototypes -> do
              forAllValid $ \user2TriggeredItemPrototypes -> do
                -- Set up an email trigger
                runPersistentTest pool $ do
                  -- One user with a trigger and the other without
                  DB.insert_ (user1 :: User)
                  DB.insert_ (user2 :: User)
                  DB.insert_
                    $ emailTrigger
                      { emailTriggerUser = userIdentifier user1,
                        emailTriggerVerified = True
                      }

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
                testRunLooper pool runTriggeredEmailScheduler

                triggeredEmailsAfterFirstRun <-
                  runPersistentTest pool
                    $ DB.selectList [] []

                testRunLooper pool runTriggeredEmailScheduler
                triggeredEmailsAfterSecondRun <-
                  runPersistentTest pool
                    $ DB.selectList [] []

                triggeredEmailsAfterSecondRun `shouldBe` (triggeredEmailsAfterFirstRun :: [DB.Entity TriggeredEmail])
