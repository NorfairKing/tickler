{-# LANGUAGE TypeApplications #-}

module Tickler.API.InstanceSpec
  ( spec,
  )
where

import Test.Syd.Validity.Aeson
import TestImport
import Tickler.API.Gen ()
import Tickler.API.Types

spec :: Spec
spec = do
  genValidSpec @Registration
  jsonSpec @Registration
  genValidSpec @LoginForm
  jsonSpec @LoginForm
  genValidSpec @ChangePassphrase
  jsonSpec @ChangePassphrase
  genValidSpec @Pricing
  jsonSpec @Pricing
