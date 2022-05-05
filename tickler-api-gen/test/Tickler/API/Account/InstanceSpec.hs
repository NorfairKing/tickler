{-# LANGUAGE TypeApplications #-}

module Tickler.API.Account.InstanceSpec
  ( spec,
  )
where

import Test.Syd.Validity.Aeson
import TestImport
import Tickler.API.Account
import Tickler.API.Account.Gen ()

spec :: Spec
spec = do
  genValidSpec @AccountInfo
  jsonSpec @AccountInfo
  genValidSpec @AccountSettings
  jsonSpec @AccountSettings
  genValidSpec @PaidStatus
  jsonSpec @PaidStatus
