module Tickler.Server.Looper.TriggeredIntrayItemSchedulerSpec (spec) where

import Data.Time
import qualified Database.Persist as DB
import Test.Syd.Persistent
import TestImport
import Tickler.API
import Tickler.Client
import Tickler.Data.Gen ()
import Tickler.Server.Looper.TriggeredIntrayItemScheduler
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerDatabase $ do
  describe "runTriggeredIntrayItemScheduler" $ do
    it "it schedules these items" $ \pool ->
      forAllValid $ \user ->
        forAllValid $ \intrayTrigger ->
          forAllValid $ \triggeredItems ->
            runPersistentTest pool $ do
              -- Set up an intray trigger
              DB.insert_ (user :: User)
              DB.insert_ (intrayTrigger :: IntrayTrigger)
              DB.insert_ UserTrigger {userTriggerUserId = userIdentifier user, userTriggerTriggerType = IntrayTriggerType, userTriggerTriggerId = intrayTriggerIdentifier intrayTrigger}

              -- Set up some triggered items
              DB.insertMany_ (triggeredItems :: [TriggeredItem])

              -- Run the looper
              liftIO $ testRunLooper pool runTriggeredIntrayItemScheduler

              -- Check that the intray items were scheduled to be sent.
              triggeredIntrayItems <- DB.selectList [TriggeredIntrayItemTrigger DB.==. intrayTriggerIdentifier intrayTrigger] []
              liftIO $ map (triggeredIntrayItemItem . DB.entityVal) triggeredIntrayItems `shouldBe` map triggeredItemIdentifier triggeredItems

    pending "it does not schedule these items"
  describe "scheduleTriggeredIntrayItem" $ do
    pending "it schedules this item"
    pending "it does not schedule this item"
