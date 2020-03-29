{-# LANGUAGE TypeApplications #-}

module Tickler.API.Admin.InstanceSpec
  ( spec
  ) where

import TestImport

import Test.Validity.Aeson

import Tickler.API.Admin.Types

import Tickler.API.Admin.Gen ()

spec :: Spec
spec = do
  genValidSpec @AdminStats
  jsonSpecOnValid @AdminStats
  genValidSpec @ActiveUsers
  jsonSpecOnValid @ActiveUsers
