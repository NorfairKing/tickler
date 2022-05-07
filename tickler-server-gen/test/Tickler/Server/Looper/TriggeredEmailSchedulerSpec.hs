{-# LANGUAGE RecordWildCards #-}
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
              DB.insert_ $
                emailTrigger
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
            pure $
              ti
                { triggeredItemIdentifier = uuid,
                  triggeredItemUserId = userIdentifier user
                }

          -- Set up the triggered items
          runPersistentTest pool $ DB.insertMany_ (triggeredItems :: [TriggeredItem])

          -- Run the looper
          testRunLooper pool runTriggeredEmailScheduler

          -- Check that the email items were scheduled to be sent.
          triggeredEmails <- runPersistentTest pool $ DB.selectList [] []
          map (triggeredEmailItem . DB.entityVal) triggeredEmails `shouldBe` []

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
                  DB.insert_ $
                    emailTrigger
                      { emailTriggerUser = userIdentifier user1,
                        emailTriggerVerified = True
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
                testRunLooper pool runTriggeredEmailScheduler

                triggeredEmailsAfterFirstRun <-
                  runPersistentTest pool $
                    DB.selectList [] []
                triggeredEmailsAfterSecondRun <-
                  runPersistentTest pool $
                    DB.selectList [] []
                triggeredEmailsAfterSecondRun `shouldBe` (triggeredEmailsAfterFirstRun :: [DB.Entity TriggeredEmail])

  describe "scheduleTriggeredEmail" $ do
    it "it schedules this item" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \emailTrigger ->
          forAllValid $ \triggeredItemPrototype -> do
            runPersistentTest pool $ do
              -- Set up an email trigger
              DB.insert_ (user :: User)
              DB.insert_ $
                emailTrigger
                  { emailTriggerUser = userIdentifier user,
                    emailTriggerVerified = True
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
            testRunLooper pool $ scheduleTriggeredEmail $ DB.Entity triggeredItemId triggeredItem

            -- Check that the email items were scheduled to be sent.
            mTriggeredEmail <-
              runPersistentTest pool $
                DB.selectFirst [TriggeredEmailTrigger DB.==. emailTriggerIdentifier emailTrigger] []
            case mTriggeredEmail of
              Nothing -> expectationFailure "Should have found a triggered email item."
              Just (DB.Entity _ TriggeredEmail {..}) -> triggeredEmailItem `shouldBe` triggeredItemIdentifier triggeredItem

    it "it does not schedule an item without a trigger" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \triggeredItemPrototype -> do
          -- Set up an email trigger
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
          testRunLooper pool $ scheduleTriggeredEmail $ DB.Entity triggeredItemId triggeredItem

          -- Check that the email items were scheduled to be sent.
          mTriggeredEmail <- runPersistentTest pool $ DB.selectFirst [] []
          case mTriggeredEmail of
            Nothing -> pure ()
            Just (_ :: DB.Entity TriggeredEmail) -> expectationFailure "Should not have found a triggered email item."

    it "it does not schedule an item with an unverified trigger" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \emailTrigger ->
          forAllValid $ \triggeredItemPrototype -> do
            -- Set up an email trigger
            runPersistentTest pool $ do
              DB.insert_ (user :: User)
              DB.insert_ $
                emailTrigger
                  { emailTriggerUser = userIdentifier user,
                    emailTriggerVerified = False
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
            testRunLooper pool $ scheduleTriggeredEmail $ DB.Entity triggeredItemId triggeredItem

            -- Check that the email items were scheduled to be sent.
            mTriggeredEmail <- runPersistentTest pool $ DB.selectFirst [] []
            case mTriggeredEmail of
              Nothing -> pure ()
              Just (_ :: DB.Entity TriggeredEmail) -> expectationFailure "Should not have found a triggered email item."

    it "it does not schedule another user's item" $ \pool ->
      forAllValid $ \user1 ->
        forAllValid $ \user2 ->
          forAllValid $ \emailTrigger ->
            forAllValid $ \triggeredItemPrototype -> do
              runPersistentTest pool $ do
                -- Set up an email trigger
                DB.insert_ (user1 :: User)
                DB.insert_ (user2 :: User)
                DB.insert_
                  ( emailTrigger
                      { emailTriggerUser = userIdentifier user1,
                        emailTriggerVerified = True
                      }
                  )

              -- Make sure the triggered item has a unique uuid and belongs to the user
              uuid <- nextRandomUUID
              let triggeredItem =
                    triggeredItemPrototype
                      { triggeredItemIdentifier = uuid,
                        triggeredItemUserId = userIdentifier user2
                      }

              -- Set up the triggered item
              triggeredItemId <- runPersistentTest pool $ DB.insert (triggeredItem :: TriggeredItem)

              -- Run the looper
              testRunLooper pool $ scheduleTriggeredEmail $ DB.Entity triggeredItemId triggeredItem

              -- Check that the email items were scheduled to be sent.
              mTriggeredEmail <-
                runPersistentTest pool $
                  DB.selectFirst [TriggeredEmailTrigger DB.==. emailTriggerIdentifier emailTrigger] []
              case mTriggeredEmail of
                Nothing -> pure ()
                Just _ -> expectationFailure "Should not have triggered this item."
