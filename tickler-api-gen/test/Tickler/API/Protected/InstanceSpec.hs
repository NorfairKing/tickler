{-# LANGUAGE TypeApplications #-}

module Tickler.API.Protected.InstanceSpec
  ( spec,
  )
where

import Test.Syd.Validity.Aeson
import TestImport
import Tickler.API.Protected.Gen ()
import Tickler.API.Protected.Types

spec :: Spec
spec = do
  genValidSpec @(ItemInfo ByteString)
  genValidSpec @TypedItem
  jsonSpec @TypedItem
  jsonSpec @TypedItem
  genValidSpec @(Tickle TypedItem)
  jsonSpec @(Tickle TypedItem)
  genValidSpec @(ItemInfo TypedItem)
  jsonSpec @(ItemInfo TypedItem)
  genValidSpec @AddItem
  jsonSpec @AddItem
  genValidSpec @TypedTriggerInfo
  jsonSpec @TypedTriggerInfo
  genValidSpec @(TriggerInfo TypedTriggerInfo)
  jsonSpec @(TriggerInfo TypedTriggerInfo)
  genValidSpec @IntrayTriggerInfo
  jsonSpec @IntrayTriggerInfo
  genValidSpec @EmailTriggerInfo
  jsonSpec @EmailTriggerInfo
  genValidSpec @AddIntrayTrigger
  jsonSpec @AddIntrayTrigger
  genValidSpec @AddEmailTrigger
  jsonSpec @AddEmailTrigger
