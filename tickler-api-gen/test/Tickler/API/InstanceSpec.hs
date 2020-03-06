{-# LANGUAGE TypeApplications #-}

module Tickler.API.InstanceSpec
  ( spec
  ) where

import TestImport

import Test.Validity.Aeson

import Tickler.API.Gen ()
import Tickler.API.Types

spec :: Spec
spec = do
  genValidSpec @Registration
  jsonSpecOnValid @Registration
  genValidSpec @LoginForm
  jsonSpecOnValid @LoginForm
