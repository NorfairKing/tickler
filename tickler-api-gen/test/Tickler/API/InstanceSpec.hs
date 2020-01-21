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
  eqSpecOnValid @Registration
  ordSpecOnValid @Registration
  genValidSpec @Registration
  jsonSpecOnValid @Registration
  eqSpecOnValid @LoginForm
  ordSpecOnValid @LoginForm
  genValidSpec @LoginForm
  jsonSpecOnValid @LoginForm
