{-# LANGUAGE TypeApplications #-}

module Tickler.Data.InstanceSpec
  ( spec,
  )
where

import qualified Intray.Data as Intray
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Persist
import TestImport
import Tickler.Data
import Tickler.Data.Gen ()

spec :: Spec
spec = do
  genValidSpec @(UUID Int)
  jsonSpec @(UUID Int)
  genValidSpec @Username
  jsonSpec @Username
  persistSpec @Username
  genValidSpec @HashedPassword
  persistSpec @HashedPassword
  genValidSpec @User
  persistSpec @User
  genValidSpec @UserSettings
  xdescribe "does not hold" $ persistSpec @UserSettings
  genValidSpec @TriggerType
  persistSpec @TriggerType
  genValidSpec @Recurrence
  jsonSpec @Recurrence
  persistSpec @Recurrence
  describe "Recurrence" $ do
    describe "everyDaysAtTime" $ it "produces valid recurrences" $ producesValid2 everyDaysAtTime
    describe "everyMonthsOnDayAtTime" $
      it "produces valid recurrences" $
        producesValid3 everyMonthsOnDayAtTime
  genValidSpec @TicklerItem
  persistSpec @TicklerItem
  genValidSpec @TriggeredItem
  persistSpec @TriggeredItem
  genValidSpec @UserTrigger
  persistSpec @UserTrigger
  genValidSpec @BaseUrl
  jsonSpec @BaseUrl
  persistSpec @BaseUrl
  genValidSpec @Intray.Username
  jsonSpec @Intray.Username
  genValidSpec @Intray.AccessKeySecret
  jsonSpec @Intray.AccessKeySecret
  genValidSpec @IntrayTrigger
  persistSpec @IntrayTrigger
  genValidSpec @EmailAddress
  jsonSpec @EmailAddress
  persistSpec @EmailAddress
  genValidSpec @EmailVerificationKey
  showReadSpec @EmailVerificationKey
  persistSpec @EmailVerificationKey
  describe "Encode and Decode EmailVerificationKey" $
    it "are inverses" $
      forAllValid $
        \evk ->
          parseEmailVerificationKeyText (emailVerificationKeyText evk) `shouldBe` Just evk
  genValidSpec @EmailTrigger
  persistSpec @EmailTrigger
  genValidSpec @VerificationEmail
  persistSpec @VerificationEmail
  genValidSpec @EmailStatus
  persistSpec @EmailStatus
  genValidSpec @Email
  persistSpec @Email
