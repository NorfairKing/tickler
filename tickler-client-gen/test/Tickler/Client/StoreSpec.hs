{-# LANGUAGE TypeApplications #-}

module Tickler.Client.StoreSpec
  ( spec,
  )
where

import TestImport
import Tickler.Client.Gen ()
import Tickler.Client.Store

spec :: Spec
spec = do
  genValidSpec @Store
  jsonSpec @Store
  describe "makeSyncRequest" $
    it "produces valid sync requests" $
      producesValid makeSyncRequest
  describe "mergeSyncResponse" $
    it "produces valid sync requests" $
      producesValid2 mergeSyncResponse
