{-# LANGUAGE TypeApplications #-}

module Tickler.API.Account.InstanceSpec
  ( spec
  ) where

import TestImport

import System.IO

import Test.Validity.Aeson

import Tickler.API.Account.Gen ()
import Tickler.API.Account.Types

spec :: Spec
spec = do
  runIO $ hSetBuffering stdout NoBuffering
  genValidSpec @AccountInfo
  jsonSpecOnValid @AccountInfo
  genValidSpec @AccountSettings
  jsonSpecOnValid @AccountSettings
