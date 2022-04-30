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
  genValidSpec @ItemInfo
  jsonSpec @ItemInfo
  genValidSpec @Tickle
  jsonSpec @Tickle
  genValidSpec @IntrayTriggerInfo
  jsonSpec @IntrayTriggerInfo
  genValidSpec @EmailTriggerInfo
  jsonSpec @EmailTriggerInfo
  genValidSpec @AddIntrayTrigger
  jsonSpec @AddIntrayTrigger
  genValidSpec @AddEmailTrigger
  jsonSpec @AddEmailTrigger
