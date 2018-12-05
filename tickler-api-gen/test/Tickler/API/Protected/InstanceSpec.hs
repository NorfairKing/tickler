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
    eqSpecOnValid @(ItemInfo ByteString)
    ordSpecOnValid @(ItemInfo ByteString)
    genValidSpec @(ItemInfo ByteString)
    eqSpecOnValid @TypedItem
    ordSpecOnValid @TypedItem
    genValidSpec @TypedItem
    jsonSpecOnValid @TypedItem
    jsonSpecOnValid @TypedItem
    eqSpecOnValid @(Tickle TypedItem)
    ordSpecOnValid @(Tickle TypedItem)
    genValidSpec @(Tickle TypedItem)
    jsonSpecOnValid @(Tickle TypedItem)
    eqSpecOnValid @(ItemInfo TypedItem)
    ordSpecOnValid @(ItemInfo TypedItem)
    genValidSpec @(ItemInfo TypedItem)
    jsonSpecOnValid @(ItemInfo TypedItem)
    eqSpecOnValid @AddItem
    ordSpecOnValid @AddItem
    genValidSpec @AddItem
    jsonSpecOnValid @AddItem
    eqSpecOnValid @TypedTriggerInfo
    genValidSpec @TypedTriggerInfo
    jsonSpecOnValid @TypedTriggerInfo
    eqSpecOnValid @(TriggerInfo TypedTriggerInfo)
    genValidSpec @(TriggerInfo TypedTriggerInfo)
    jsonSpecOnValid @(TriggerInfo TypedTriggerInfo)
    eqSpecOnValid @IntrayTriggerInfo
    ordSpecOnValid @IntrayTriggerInfo
    genValidSpec @IntrayTriggerInfo
    jsonSpecOnValid @IntrayTriggerInfo
    eqSpecOnValid @EmailTriggerInfo
    ordSpecOnValid @EmailTriggerInfo
    genValidSpec @EmailTriggerInfo
    jsonSpecOnValid @EmailTriggerInfo
    eqSpecOnValid @AddIntrayTrigger
    ordSpecOnValid @AddIntrayTrigger
    genValidSpec @AddIntrayTrigger
    jsonSpecOnValid @AddIntrayTrigger
    eqSpecOnValid @AddEmailTrigger
    ordSpecOnValid @AddEmailTrigger
    genValidSpec @AddEmailTrigger
    jsonSpecOnValid @AddEmailTrigger
    eqSpecOnValid @SyncRequest
    ordSpecOnValid @SyncRequest
    genValidSpec @SyncRequest
    jsonSpecOnValid @SyncRequest
    eqSpecOnValid @SyncResponse
    ordSpecOnValid @SyncResponse
    genValidSpec @SyncResponse
    jsonSpecOnValid @SyncResponse
