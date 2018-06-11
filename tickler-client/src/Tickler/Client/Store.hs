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
import qualified Data.Set as S

import Tickler.API

data Store = Store
    { storeTickles :: Mergeless.Store ItemUUID TypedTickle
    } deriving (Show, Eq, Generic)

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
    Store
    { storeTickles =
          Mergeless.mergeSyncResponse storeTickles syncResponseTickles
    }

emptyStore :: Store
emptyStore = Store {storeTickles = Mergeless.emptyStore}

addTickleToStore :: Store -> Added TypedTickle -> Store
addTickleToStore Store {..} a =
    Store
    { storeTickles =
          Mergeless.Store $
          S.insert (Mergeless.UnsyncedItem a) $
          Mergeless.storeItems storeTickles
    }
