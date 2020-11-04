{-# LANGUAGE TypeApplications #-}

module Tickler.API.Protected.InstanceSpec
  ( spec,
  )
where

import Test.Validity.Aeson
import TestImport
import Tickler.API.Protected.Gen ()
import Tickler.API.Protected.Types

spec :: Spec
spec = do
  genValidSpec @(ItemInfo ByteString)
  genValidSpec @TypedItem
  jsonSpecOnValid @TypedItem
  jsonSpecOnValid @TypedItem
  genValidSpec @(Tickle TypedItem)
  jsonSpecOnValid @(Tickle TypedItem)
  genValidSpec @(ItemInfo TypedItem)
  jsonSpecOnValid @(ItemInfo TypedItem)
  genValidSpec @AddItem
  jsonSpecOnValid @AddItem
  genValidSpec @TypedTriggerInfo
  jsonSpecOnValid @TypedTriggerInfo
  genValidSpec @(TriggerInfo TypedTriggerInfo)
  jsonSpecOnValid @(TriggerInfo TypedTriggerInfo)
  genValidSpec @IntrayTriggerInfo
  jsonSpecOnValid @IntrayTriggerInfo
  genValidSpec @EmailTriggerInfo
  jsonSpecOnValid @EmailTriggerInfo
  genValidSpec @AddIntrayTrigger
  jsonSpecOnValid @AddIntrayTrigger
  genValidSpec @AddEmailTrigger
  jsonSpecOnValid @AddEmailTrigger
  genValidSpec @SyncRequest
  jsonSpecOnValid @SyncRequest
  genValidSpec @SyncResponse
  jsonSpecOnValid @SyncResponse
