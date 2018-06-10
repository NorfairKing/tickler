{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Client.Store
    ( Store(..)
    , makeSyncRequest
    , mergeSyncResponse
    , emptyStore
    ) where

import Import

import Data.Aeson
import qualified Data.Mergeless as Mergeless

import Tickler.API

data Store = Store
    { storeTickles :: Mergeless.Store ItemUUID TypedTickle
    } deriving (Show, Eq, Generic)

instance Validity Store

instance FromJSON Store

instance ToJSON Store

makeSyncRequest :: Store -> SyncRequest
makeSyncRequest Store {..} =
    SyncRequest {syncRequestTickles = Mergeless.makeSyncRequest storeTickles}

mergeSyncResponse :: Store -> SyncResponse -> Store
mergeSyncResponse Store {..} SyncResponse {..} =
    Store
    { storeTickles =
          Mergeless.mergeSyncResponse storeTickles syncResponseTickles
    }

emptyStore :: Store
emptyStore = Store {storeTickles = Mergeless.emptyStore}
