{-# LANGUAGE TypeApplications #-}

module Tickler.Client.StoreSpec
  ( spec
  ) where

import TestImport

import Tickler.Client.Gen ()

import Tickler.Client.Store

spec :: Spec
spec = do
  eqSpecOnValid @Store
  genValidSpec @Store
  jsonSpecOnValid @Store
  describe "makeSyncRequest" $
    it "produces valid sync requests" $ producesValidsOnValids makeSyncRequest
  describe "mergeSyncResponse" $
    it "produces valid sync requests" $ producesValidsOnValids2 mergeSyncResponse
