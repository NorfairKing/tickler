{-# LANGUAGE TypeApplications #-}

module Tickler.Data.InstanceSpec
  ( spec
  ) where

import TestImport

import Test.Validity.Aeson

import qualified Intray.Data as Intray

import Tickler.Data

import Tickler.Data.Gen ()

spec :: Spec
spec = do
  genValidSpec @(UUID Int)
  jsonSpecOnValid @(UUID Int)
  genValidSpec @Username
  jsonSpecOnValid @Username
  genValidSpec @HashedPassword
  genValidSpec @User
  genValidSpec @UserSettings
  jsonSpecOnValid @ItemType
  genValidSpec @ItemType
  genValidSpec @TriggerType
  genValidSpec @Recurrence
  jsonSpecOnValid @Recurrence
  describe "Recurrence" $ do
    describe "everyDaysAtTime" $ it "produces valid recurrences" $ producesValid2 everyDaysAtTime
    describe "everyMonthsOnDayAtTime" $
      it "produces valid recurrences" $ producesValid3 everyMonthsOnDayAtTime
  genValidSpec @TicklerItem
  genValidSpec @TriggeredItem
  genValidSpec @UserTrigger
  genValidSpec @BaseUrl
  jsonSpecOnValid @BaseUrl
  genValidSpec @Intray.Username
  jsonSpecOnValid @Intray.Username
  genValidSpec @Intray.AccessKeySecret
  jsonSpecOnValid @Intray.AccessKeySecret
  genValidSpec @IntrayTrigger
  genValidSpec @EmailAddress
  jsonSpecOnValid @EmailAddress
  genValidSpec @EmailVerificationKey
  showReadSpecOnValid @EmailVerificationKey
  describe "Encode and Decode EmailVerificationKey" $
    it "are inverses" $
    forAllValid $ \evk ->
      parseEmailVerificationKeyText (emailVerificationKeyText evk) `shouldBe` Just evk
  genValidSpec @EmailTrigger
  genValidSpec @VerificationEmail
  genValidSpec @EmailStatus
  genValidSpec @Email
