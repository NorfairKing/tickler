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
    eqSpecOnValid @AccountInfo
    ordSpecOnValid @AccountInfo
    genValidSpec @AccountInfo
    jsonSpecOnValid @AccountInfo
    eqSpecOnValid @AccountSettings
    ordSpecOnValid @AccountSettings
    genValidSpec @AccountSettings
    jsonSpecOnValid @AccountSettings
