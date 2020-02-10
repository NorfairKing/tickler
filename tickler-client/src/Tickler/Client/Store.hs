{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Client.Store
  ( Store(..)
  , makeSyncRequest
  , mergeSyncResponse
  , emptyStore
  , addTickleToStore
  ) where

import Import

import Data.Aeson
import qualified Data.Mergeless as Mergeless

import Tickler.API

data Store =
  Store
    { storeTickles :: Mergeless.ClientStore ItemUUID (AddedItem TypedTickle)
    }
  deriving (Show, Eq, Generic)

instance Validity Store

instance FromJSON Store where
  parseJSON = withObject "Store" $ \o -> Store <$> o .: "tickles"

instance ToJSON Store where
  toJSON Store {..} = object ["tickles" .= storeTickles]

makeSyncRequest :: Store -> SyncRequest
makeSyncRequest Store {..} =
  SyncRequest {syncRequestTickles = Mergeless.makeSyncRequest storeTickles}

mergeSyncResponse :: Store -> SyncResponse -> Store
mergeSyncResponse Store {..} SyncResponse {..} =
  Store {storeTickles = Mergeless.mergeSyncResponse storeTickles syncResponseTickles}

emptyStore :: Store
emptyStore = Store {storeTickles = Mergeless.emptyClientStore}

addTickleToStore :: Store -> AddedItem TypedTickle -> Store
addTickleToStore Store {..} a = Store {storeTickles = Mergeless.addItemToClientStore a storeTickles}
