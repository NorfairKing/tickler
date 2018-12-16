module Tickler.Server.Looper.TriggererSpec where

import TestImport

import Data.Time

import Tickler.Data
import Tickler.Server.Looper.Triggerer

import Tickler.Data.Gen ()

spec :: Spec
spec = do
    describe "makeTriggeredItem" $
        it "produces valid ticklerItems when it succeeds" $
        producesValidsOnValids2 makeTriggeredItem
    describe "nextScheduledTime" $ do
        it "produces valid utc times" $
            producesValidsOnValids3 nextScheduledTime
        it "never produces the same scheduled time and day" $
            forAllValid $ \d ->
                forAllValid $ \mt ->
                    forAllValid $ \r ->
                        nextScheduledTime d mt r `shouldNotBe` (d, mt)
        it "works for this simple example" $
            nextScheduledTime
                (fromGregorian 1970 01 01)
                (Just midday)
                (EveryDaysAtTime 5 (Just midnight)) `shouldBe`
            (fromGregorian 1970 01 06, Just midnight)
        it "works for this simple example" $
            nextScheduledTime
                (fromGregorian 1970 01 01)
                (Just midnight)
                (EveryMonthsOnDay 4 (Just 7) (Just midday)) `shouldBe`
            (fromGregorian 1970 05 07, Just midday)
    describe "makeNextTickleItem" $ do
        it "produces valid ticklerItems when it succeeds" $
            forAllValid $ \ti -> do
                mti' <- makeNextTickleItem ti
                shouldBeValid mti'
        it "produces Nothing if there is no recurrence" $
            forAll ((\ti -> ti {ticklerItemRecurrence = Nothing}) <$> genValid) $ \ti ->
                makeNextTickleItem ti `shouldReturn` Nothing
        it "never produces the same tickle item" $
            forAllValid $ \ti ->
                makeNextTickleItem ti `shouldNotReturn` (Just ti)
        it "has a seperate ID" $
            forAllValid $ \ti -> do
                mti' <- makeNextTickleItem ti
                case mti' of
                    Nothing -> pure ()
                    Just ti' -> do
                        ti' `shouldNotBe` ti
                        ticklerItemIdentifier ti' `shouldNotBe`
                            ticklerItemIdentifier ti
