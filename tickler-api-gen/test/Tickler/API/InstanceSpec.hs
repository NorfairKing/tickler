{-# LANGUAGE TypeApplications #-}

module Tickler.API.InstanceSpec
  ( spec
  ) where

import Test.Validity.Aeson
import TestImport
import Tickler.API.Gen ()
import Tickler.API.Types

spec :: Spec
spec = do
  genValidSpec @Registration
  jsonSpecOnValid @Registration
  genValidSpec @LoginForm
  jsonSpecOnValid @LoginForm
  genValidSpec @ChangePassphrase
  jsonSpecOnValid @ChangePassphrase
  genValidSpec @Pricing
  jsonSpecOnValid @Pricing
