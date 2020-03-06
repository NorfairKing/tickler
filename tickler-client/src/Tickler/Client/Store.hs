{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Client.Store where

import Import

import Data.Aeson
import qualified Data.Mergeful as Mergeful

import Tickler.API

data Store =
  Store
    { storeTickles :: Mergeful.ClientStore ItemUUID (AddedItem TypedTickle)
    }
  deriving (Show, Eq, Generic)

instance Validity Store

instance FromJSON Store where
  parseJSON = withObject "Store" $ \o -> Store <$> o .: "tickles"

instance ToJSON Store where
  toJSON Store {..} = object ["tickles" .= storeTickles]

makeSyncRequest :: Store -> SyncRequest
makeSyncRequest Store {..} =
  SyncRequest {syncRequestTickles = Mergeful.makeSyncRequest storeTickles}

mergeSyncResponse :: Store -> SyncResponse -> Store
mergeSyncResponse Store {..} SyncResponse {..} =
  Store {storeTickles = Mergeful.mergeSyncResponseFromServer storeTickles syncResponseTickles}

emptyStore :: Store
emptyStore = Store {storeTickles = Mergeful.initialClientStore}

addTickleToStore :: Store -> AddedItem TypedTickle -> Store
addTickleToStore Store {..} a = Store {storeTickles = Mergeful.addItemToClientStore a storeTickles}
