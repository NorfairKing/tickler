{-# LANGUAGE TypeApplications #-}

module Tickler.API.Account.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Tickler.API.Account.Gen ()
import Tickler.API.Account.Types

spec :: Spec
spec = do
    eqSpec @AccountInfo
    ordSpec @AccountInfo
    genValidSpec @AccountInfo
    jsonSpecOnValid @AccountInfo
    eqSpec @AccountSettings
    ordSpec @AccountSettings
    genValidSpec @AccountSettings
    jsonSpecOnValid @AccountSettings
