{-# LANGUAGE TypeApplications #-}

module Tickler.Data.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Tickler.Data

import Tickler.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @(UUID Int)
    ordSpec @(UUID Int)
    genValidSpec @(UUID Int)
    jsonSpecOnValid @(UUID Int)
    eqSpec @Username
    ordSpec @Username
    genValidSpec @Username
    jsonSpecOnValid @Username
    eqSpec @HashedPassword
    genValidSpec @HashedPassword
    eqSpec @User
    ordSpec @User
    genValidSpec @User
    eqSpec @UserSettings
    ordSpec @UserSettings
    genValidSpec @UserSettings
    eqSpec @ItemType
    ordSpec @ItemType
    jsonSpecOnValid @ItemType
    genValidSpec @ItemType
    eqSpec @TriggerType
    ordSpec @TriggerType
    genValidSpec @TriggerType
    eqSpec @Recurrence
    ordSpec @Recurrence
    genValidSpec @Recurrence
    jsonSpecOnValid @Recurrence
    describe "Recurrence" $ do
        describe "nominal" $
            it "produces valid recurrences" $ producesValid nominal
        describe "everyDaysAtTime" $
            it "produces valid recurrences" $ producesValid2 everyDaysAtTime
        describe "everyDayAtTime" $
            it "produces valid recurrences" $ producesValid everyDayAtTime
        describe "everyDays" $
            it "produces valid recurrences" $ producesValid everyDays
        describe "everyDay" $ it "is valid" $ shouldBeValid everyDay
        describe "everyMonths" $
            it "produces valid recurrences" $ producesValid everyMonths
        describe "everyMonth" $ it "is valid" $ shouldBeValid everyMonth
    eqSpec @TicklerItem
    ordSpec @TicklerItem
    genValidSpec @TicklerItem
    eqSpec @TriggeredItem
    ordSpec @TriggeredItem
    genValidSpec @TriggeredItem
    eqSpec @UserTrigger
    ordSpec @UserTrigger
    genValidSpec @UserTrigger
    eqSpec @BaseUrl
    ordSpec @BaseUrl
    genValidSpec @BaseUrl
    jsonSpecOnValid @BaseUrl
    eqSpec @IntrayTrigger
    ordSpec @IntrayTrigger
    genValidSpec @IntrayTrigger
    eqSpec @EmailAddress
    ordSpec @EmailAddress
    genValidSpec @EmailAddress
    jsonSpecOnValid @EmailAddress
    eqSpec @EmailVerificationKey
    ordSpec @EmailVerificationKey
    genValidSpec @EmailVerificationKey
    describe "Encode and Decode EmailVerificationKey" $
        it "are inverses" $
        forAllUnchecked $ \evk ->
            parseEmailVerificationKeyText (emailVerificationKeyText evk) `shouldBe`
            Just evk
    eqSpec @EmailTrigger
    ordSpec @EmailTrigger
    genValidSpec @EmailTrigger
    eqSpec @VerificationEmail
    ordSpec @VerificationEmail
    genValidSpec @VerificationEmail
    eqSpec @EmailStatus
    ordSpec @EmailStatus
    genValidSpec @EmailStatus
    eqSpec @Email
    ordSpec @Email
    genValidSpec @Email
