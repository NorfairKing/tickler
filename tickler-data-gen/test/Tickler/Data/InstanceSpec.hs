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
    eqSpecOnValid @(UUID Int)
    ordSpecOnValid @(UUID Int)
    genValidSpec @(UUID Int)
    jsonSpecOnValid @(UUID Int)
    eqSpecOnValid @Username
    ordSpecOnValid @Username
    genValidSpec @Username
    jsonSpecOnValid @Username
    eqSpecOnValid @HashedPassword
    genValidSpec @HashedPassword
    eqSpecOnValid @User
    ordSpecOnValid @User
    genValidSpec @User
    eqSpecOnValid @UserSettings
    ordSpecOnValid @UserSettings
    genValidSpec @UserSettings
    eqSpecOnValid @ItemType
    ordSpecOnValid @ItemType
    jsonSpecOnValid @ItemType
    genValidSpec @ItemType
    eqSpecOnValid @TriggerType
    ordSpecOnValid @TriggerType
    genValidSpec @TriggerType
    eqSpecOnValid @Recurrence
    ordSpecOnValid @Recurrence
    genValidSpec @Recurrence
    jsonSpecOnValid @Recurrence
    describe "Recurrence" $ do
        describe "everyDaysAtTime" $
            it "produces valid recurrences" $ producesValid2 everyDaysAtTime
        describe "everyMonthsOnDayAtTime" $
            it "produces valid recurrences" $
            producesValid3 everyMonthsOnDayAtTime
    eqSpecOnValid @TicklerItem
    ordSpecOnValid @TicklerItem
    genValidSpec @TicklerItem
    eqSpecOnValid @TriggeredItem
    ordSpecOnValid @TriggeredItem
    genValidSpec @TriggeredItem
    eqSpecOnValid @UserTrigger
    ordSpecOnValid @UserTrigger
    genValidSpec @UserTrigger
    eqSpecOnValid @BaseUrl
    ordSpecOnValid @BaseUrl
    genValidSpec @BaseUrl
    jsonSpecOnValid @BaseUrl
    eqSpecOnValid @Intray.Username
    ordSpecOnValid @Intray.Username
    genValidSpec @Intray.Username
    jsonSpecOnValid @Intray.Username
    eqSpecOnValid @Intray.AccessKeySecret
    ordSpecOnValid @Intray.AccessKeySecret
    genValidSpec @Intray.AccessKeySecret
    jsonSpecOnValid @Intray.AccessKeySecret
    eqSpecOnValid @IntrayTrigger
    ordSpecOnValid @IntrayTrigger
    genValidSpec @IntrayTrigger
    eqSpecOnValid @EmailAddress
    ordSpecOnValid @EmailAddress
    genValidSpec @EmailAddress
    jsonSpecOnValid @EmailAddress
    eqSpecOnValid @EmailVerificationKey
    ordSpecOnValid @EmailVerificationKey
    genValidSpec @EmailVerificationKey
    showReadSpecOnValid @EmailVerificationKey
    describe "Encode and Decode EmailVerificationKey" $
        it "are inverses" $
        forAllValid $ \evk ->
            parseEmailVerificationKeyText (emailVerificationKeyText evk) `shouldBe`
            Just evk
    eqSpecOnValid @EmailTrigger
    ordSpecOnValid @EmailTrigger
    genValidSpec @EmailTrigger
    eqSpecOnValid @VerificationEmail
    ordSpecOnValid @VerificationEmail
    genValidSpec @VerificationEmail
    eqSpecOnValid @EmailStatus
    ordSpecOnValid @EmailStatus
    genValidSpec @EmailStatus
    eqSpecOnValid @Email
    ordSpecOnValid @Email
    genValidSpec @Email
