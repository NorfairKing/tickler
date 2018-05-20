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
