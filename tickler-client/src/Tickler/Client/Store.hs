{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Client.Store
    ( Store(..)
    , StoreItem(..)
    , emptyStore
    , addItemToStore
    , LastItem(..)
    , lastItemInStore
    , doneLastItem
    , storeSize
    -- * Syncing
    , makeSyncRequest
    , mergeStore
    ) where

import Import

import Data.Aeson
import qualified Data.Set as S
import Data.Time

import Tickler.API

{-# ANN module ("HLint: ignore Use &&" :: Text) #-}

{-# ANN module ("HLint: ignore Use lambda-case" :: Text) #-}

newtype Store = Store
    { storeItems :: Set StoreItem
    } deriving (Show, Eq, Generic)

instance Validity Store

instance FromJSON Store where
    parseJSON v = Store <$> parseJSON v

instance ToJSON Store where
    toJSON = toJSON . storeItems

data StoreItem
    = Unsynced TypedItem
               UTCTime
    | Synced (ItemInfo TypedItem)
    | Undeleted ItemUUID
    deriving (Show, Eq, Ord, Generic)

instance Validity StoreItem

instance FromJSON StoreItem where
    parseJSON =
        withObject "StoreItem" $ \o -> do
            state <- o .: "state"
            case state of
                IsUnsynced -> Unsynced <$> o .: "contents" <*> o .: "timestamp"
                IsSynced -> Synced <$> o .: "item"
                IsUndeleted -> Undeleted <$> o .: "uuid"

instance ToJSON StoreItem where
    toJSON (Unsynced i ts) =
        object ["state" .= IsUnsynced, "contents" .= i, "timestamp" .= ts]
    toJSON (Synced i) = object ["state" .= IsSynced, "item" .= i]
    toJSON (Undeleted u) = object ["state" .= IsUndeleted, "uuid" .= u]

data SyncState
    = IsUnsynced
    | IsSynced
    | IsUndeleted
    deriving (Show, Eq, Generic)

instance Validity SyncState

instance FromJSON SyncState

instance ToJSON SyncState

emptyStore :: Store
emptyStore = Store S.empty

addItemToStore :: TypedItem -> UTCTime -> Store -> Store
addItemToStore contents timestamp (Store is) =
    Store $ S.insert (Unsynced contents timestamp) is

lastItemInStore :: Store -> Maybe LastItem
lastItemInStore (Store is) =
    let ls =
            flip mapSetMaybe is $ \ii ->
                case ii of
                    Unsynced t ts ->
                        Just
                            LastItem
                            { lastItemData = t
                            , lastItemTimestamp = ts
                            , lastItemUUID = Nothing
                            }
                    Synced ItemInfo {..} ->
                        Just
                            LastItem
                            { lastItemData = itemInfoContents
                            , lastItemTimestamp = itemInfoTimestamp
                            , lastItemUUID = Just itemInfoIdentifier
                            }
                    Undeleted _ -> Nothing
    in fst <$> S.minView ls

data LastItem = LastItem
    { lastItemData :: TypedItem
    , lastItemTimestamp :: UTCTime
    , lastItemUUID :: Maybe ItemUUID
    } deriving (Show, Eq, Ord, Generic)

instance FromJSON LastItem

instance ToJSON LastItem

-- TODO maybe do this with an internal uuid?
doneLastItem :: LastItem -> Store -> Store
doneLastItem LastItem {..} (Store is) =
    Store $
    flip mapSetMaybe is $ \si ->
        case si of
            Unsynced t ts ->
                if and [t == lastItemData, ts == lastItemTimestamp]
                    then Nothing
                    else Just si
            Synced ItemInfo {..} ->
                if and [ itemInfoContents == lastItemData
                       , itemInfoTimestamp == lastItemTimestamp
                       , Just itemInfoIdentifier == lastItemUUID
                       ]
                    then Just (Undeleted itemInfoIdentifier)
                    else Just si
            Undeleted _ -> Just si

storeSize :: Store -> Int
storeSize (Store is) =
    length $
    flip S.filter is $ \si ->
        case si of
            Unsynced _ _ -> True
            Synced _ -> True
            Undeleted _ -> False

makeSyncRequest :: Store -> SyncRequest
makeSyncRequest Store {..} =
    SyncRequest
    { syncRequestUnsyncedItems =
          S.toList $
          flip mapSetMaybe storeItems $ \si ->
              case si of
                  Unsynced t ts ->
                      Just
                          NewSyncItem
                          { newSyncItemContents = t
                          , newSyncItemTimestamp = Just ts
                          }
                  _ -> Nothing
    , syncRequestSyncedItems =
          S.toList $
          flip mapSetMaybe storeItems $ \si ->
              case si of
                  Synced ii -> Just $ itemInfoIdentifier ii
                  _ -> Nothing
    , syncRequestUndeletedItems =
          S.toList $
          flip mapSetMaybe storeItems $ \si ->
              case si of
                  Undeleted uuid -> Just uuid
                  _ -> Nothing
    }

mergeStore :: Store -> SyncResponse -> Store
mergeStore s SyncResponse {..} =
    let withNewOwnItems =
            flip mapSetMaybe (storeItems s) $ \si ->
                case si of
                    Unsynced t ut ->
                        case find
                                 (\ItemInfo {..} ->
                                      t == itemInfoContents &&
                                      ut == itemInfoTimestamp)
                                 syncResponseAddedItems of
                            Nothing -> Just si -- If it wasn't added (for whatever reason), just leave it as unsynced
                            Just ii -> Just $ Synced ii -- If it was added, then it becomes synced
                    Synced ii ->
                        case find
                                 (== itemInfoIdentifier ii)
                                 syncResponseItemsToBeDeletedLocally of
                            Nothing -> Just si -- If it wasn't deleted, don't delete it.
                            Just _ -> Nothing -- If it was deleted, delete it here.
                    Undeleted _ -> Nothing -- Delete all locally deleted items after sync
        withNewOtherItems =
            withNewOwnItems `S.union`
            S.map Synced (S.fromList syncResponseNewRemoteItems)
    in Store {storeItems = withNewOtherItems}

mapSetMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
mapSetMaybe func = S.map fromJust . S.filter isJust . S.map func
