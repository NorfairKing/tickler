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
    eqSpec @Registration
    ordSpec @Registration
    genValidSpec @Registration
    jsonSpecOnValid @Registration
    eqSpec @LoginForm
    ordSpec @LoginForm
    genValidSpec @LoginForm
    jsonSpecOnValid @LoginForm
