{-# LANGUAGE TypeApplications #-}

module Tickler.API.Protected.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Tickler.API.Protected.Gen ()
import Tickler.API.Protected.Types

spec :: Spec
spec = do
    eqSpec @(ItemInfo ByteString)
    ordSpec @(ItemInfo ByteString)
    genValidSpec @(ItemInfo ByteString)
    eqSpec @TypedItem
    ordSpec @TypedItem
    genValidSpec @TypedItem
    jsonSpecOnValid @TypedItem
    jsonSpecOnValid @TypedItem
    eqSpec @(ItemInfo TypedItem)
    ordSpec @(ItemInfo TypedItem)
    genValidSpec @(ItemInfo TypedItem)
    jsonSpecOnValid @(ItemInfo TypedItem)
    eqSpec @AddItem
    ordSpec @AddItem
    genValidSpec @AddItem
    jsonSpecOnValid @AddItem
    eqSpec @SyncRequest
    ordSpec @SyncRequest
    genValidSpec @SyncRequest
    jsonSpecOnValid @SyncRequest
    eqSpec @NewSyncItem
    ordSpec @NewSyncItem
    genValidSpec @NewSyncItem
    jsonSpecOnValid @NewSyncItem
    eqSpec @SyncResponse
    ordSpec @SyncResponse
    genValidSpec @SyncResponse
    jsonSpecOnValid @SyncResponse
    eqSpec @TriggerInfo
    ordSpec @TriggerInfo
    genValidSpec @TriggerInfo
    jsonSpecOnValid @TriggerInfo
    eqSpec @AddIntrayTrigger
    ordSpec @AddIntrayTrigger
    genValidSpec @AddIntrayTrigger
    jsonSpecOnValid @AddIntrayTrigger
    eqSpec @AddEmailTrigger
    ordSpec @AddEmailTrigger
    genValidSpec @AddEmailTrigger
    jsonSpecOnValid @AddEmailTrigger
