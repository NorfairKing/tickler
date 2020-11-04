{-# LANGUAGE TypeApplications #-}

module Tickler.Data.InstanceSpec
  ( spec,
  )
where

import Data.Mergeful.Timed as Mergeful
import qualified Intray.Data as Intray
import Test.Validity.Aeson
import Test.Validity.Persist
import TestImport
import Tickler.Data
import Tickler.Data.Gen ()

spec :: Spec
spec = do
  genValidSpec @(UUID Int)
  jsonSpecOnValid @(UUID Int)
  genValidSpec @Username
  jsonSpecOnValid @Username
  persistSpecOnValid @Username
  genValidSpec @HashedPassword
  persistSpecOnValid @HashedPassword
  genValidSpec @User
  persistSpecOnValid @User
  genValidSpec @UserSettings
  jsonSpecOnValid @ItemType
  genValidSpec @ItemType
  persistSpecOnValid @ItemType
  genValidSpec @TriggerType
  persistSpecOnValid @TriggerType
  genValidSpec @Recurrence
  jsonSpecOnValid @Recurrence
  persistSpecOnValid @Recurrence
  describe "Recurrence" $ do
    describe "everyDaysAtTime" $ it "produces valid recurrences" $ producesValid2 everyDaysAtTime
    describe "everyMonthsOnDayAtTime"
      $ it "produces valid recurrences"
      $ producesValid3 everyMonthsOnDayAtTime
  genValidSpec @TicklerItem
  -- persistSpecOnValid @TicklerItem
  persistSpecOnValid @Mergeful.ServerTime
  genValidSpec @TriggeredItem
  persistSpecOnValid @TriggeredItem
  genValidSpec @UserTrigger
  persistSpecOnValid @UserTrigger
  genValidSpec @BaseUrl
  jsonSpecOnValid @BaseUrl
  persistSpecOnValid @BaseUrl
  genValidSpec @Intray.Username
  jsonSpecOnValid @Intray.Username
  genValidSpec @Intray.AccessKeySecret
  jsonSpecOnValid @Intray.AccessKeySecret
  genValidSpec @IntrayTrigger
  persistSpecOnValid @IntrayTrigger
  genValidSpec @EmailAddress
  jsonSpecOnValid @EmailAddress
  persistSpecOnValid @EmailAddress
  genValidSpec @EmailVerificationKey
  showReadSpecOnValid @EmailVerificationKey
  persistSpecOnValid @EmailVerificationKey
  describe "Encode and Decode EmailVerificationKey"
    $ it "are inverses"
    $ forAllValid
    $ \evk ->
      parseEmailVerificationKeyText (emailVerificationKeyText evk) `shouldBe` Just evk
  genValidSpec @EmailTrigger
  persistSpecOnValid @EmailTrigger
  genValidSpec @VerificationEmail
  persistSpecOnValid @VerificationEmail
  genValidSpec @EmailStatus
  persistSpecOnValid @EmailStatus
  genValidSpec @Email
  persistSpecOnValid @Email
