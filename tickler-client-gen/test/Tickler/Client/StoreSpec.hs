{-# LANGUAGE TypeApplications #-}

module Tickler.Client.StoreSpec
    ( spec
    ) where

import TestImport

import Tickler.Client.Gen ()

import Tickler.Client.Store

spec :: Spec
spec = do
    eqSpec @Store
    genValidSpec @Store
    jsonSpecOnValid @Store
    eqSpec @StoreItem
    genValidSpec @StoreItem
    jsonSpecOnValid @StoreItem
