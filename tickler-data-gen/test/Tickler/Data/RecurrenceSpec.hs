module Tickler.Data.RecurrenceSpec (spec) where

import Data.Time
import TestImport
import Tickler.Data
import Tickler.Data.Gen ()

spec :: Spec
spec = do
  describe "nextScheduledTime" $ do
    it "produces valid utc times" $ producesValid3 nextScheduledTime
    it "never produces the same scheduled time and day" $
      forAllValid $ \d ->
        forAllValid $ \mt -> forAllValid $ \r -> nextScheduledTime d mt r `shouldNotBe` (d, mt)

    it "works for this simple example" $
      nextScheduledTime
        (fromGregorian 1970 01 01)
        (Just (timeOfDayToMinuteOfDay midday))
        (EveryDaysAtTime 5 (Just (timeOfDayToMinuteOfDay midnight)))
        `shouldBe` (fromGregorian 1970 01 06, Just (timeOfDayToMinuteOfDay midnight))

    it "works for this simple example" $
      nextScheduledTime
        (fromGregorian 1970 01 01)
        (Just (timeOfDayToMinuteOfDay midnight))
        (EveryMonthsOnDay 4 (Just 7) (Just (timeOfDayToMinuteOfDay midday)))
        `shouldBe` (fromGregorian 1970 05 07, Just (timeOfDayToMinuteOfDay midday))
