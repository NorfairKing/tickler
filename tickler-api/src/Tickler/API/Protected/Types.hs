{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.API.Protected.Types
    ( ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , SyncRequest(..)
    , NewSyncItem(..)
    , SyncResponse(..)
    , Registration(..)
    , LoginForm(..)
    , GetDocsResponse(..)
    , HashedPassword
    , passwordHash
    , validatePassword
    , ItemUUID
    , module Data.UUID.Typed
    ) where

import Import

import Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import Data.List (nub)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed

import Servant.Docs

import Tickler.Data

import Tickler.API.Types

data TypedItem = TypedItem
    { itemType :: ItemType
    , itemData :: ByteString
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity TypedItem

instance FromJSON TypedItem where
    parseJSON =
        withObject "TypedItem" $ \o ->
            TypedItem <$> o .: "type" <*>
            (do t <- o .: "data"
                case Base64.decode $ SB8.pack t of
                    Left err ->
                        fail $
                        unwords
                            [ "Failed to decode base64-encoded typed item data:"
                            , err
                            ]
                    Right r -> pure r)

instance ToJSON TypedItem where
    toJSON TypedItem {..} =
        object
            ["type" .= itemType, "data" .= SB8.unpack (Base64.encode itemData)]

instance ToSample TypedItem where
    toSamples Proxy = singleSample $ TypedItem TextItem "Hello World!"

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}

typedItemCase :: TypedItem -> Either String TypedItemCase
typedItemCase TypedItem {..} =
    case itemType of
        TextItem -> left show $ CaseTextItem <$> TE.decodeUtf8' itemData

newtype TypedItemCase =
    CaseTextItem Text
    deriving (Show, Read, Eq, Ord, Generic)

data ItemInfo a = ItemInfo
    { itemInfoIdentifier :: ItemUUID
    , itemInfoContents :: a
    , itemInfoTimestamp :: UTCTime
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (ItemInfo a)

instance ToJSON a => ToJSON (ItemInfo a) where
    toJSON ItemInfo {..} =
        object
            [ "id" .= itemInfoIdentifier
            , "contents" .= itemInfoContents
            , "timestamp" .= itemInfoTimestamp
            ]

instance FromJSON a => FromJSON (ItemInfo a) where
    parseJSON =
        withObject "ItemInfo TypedItem" $ \o ->
            ItemInfo <$> o .: "id" <*> o .: "contents" <*> o .: "timestamp"

instance ToSample a => ToSample (ItemInfo a)

data SyncRequest = SyncRequest
    { syncRequestUnsyncedItems :: [NewSyncItem]
    , syncRequestSyncedItems :: [ItemUUID]
    , syncRequestUndeletedItems :: [ItemUUID]
    } deriving (Show, Eq, Ord, Generic)

instance Validity SyncRequest where
    validate SyncRequest {..} =
        mconcat
            [ annotate syncRequestUnsyncedItems "syncRequestUnsyncedItems"
            , annotate syncRequestSyncedItems "syncRequestSyncedItems"
            , annotate syncRequestUndeletedItems "syncRequestUndeletedItems"
            , check
                  (distinct syncRequestUnsyncedItems)
                  "Unsynced items are distinct"
            , check
                  (distinct syncRequestSyncedItems)
                  "Synced items are distinct"
            , check
                  (distinct syncRequestUndeletedItems)
                  "undeleted items are distinct"
            ]

instance FromJSON SyncRequest where
    parseJSON =
        withObject "SyncRequest" $ \o ->
            SyncRequest <$> o .: "unsynced" <*> o .: "synced" <*>
            o .: "undeleted"

instance ToJSON SyncRequest where
    toJSON SyncRequest {..} =
        object
            [ "unsynced" .= syncRequestUnsyncedItems
            , "synced" .= syncRequestSyncedItems
            , "undeleted" .= syncRequestUndeletedItems
            ]

instance ToSample SyncRequest

data NewSyncItem = NewSyncItem
    { newSyncItemContents :: TypedItem
    , newSyncItemTimestamp :: Maybe UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity NewSyncItem

instance FromJSON NewSyncItem where
    parseJSON v =
        (NewSyncItem <$> parseJSON v <*> pure Nothing) <|>
        withObject
            "NewSyncItem"
            (\o -> NewSyncItem <$> o .: "contents" <*> o .:? "timestamp")
            v

instance ToJSON NewSyncItem where
    toJSON NewSyncItem {..} =
        case newSyncItemTimestamp of
            Nothing -> toJSON newSyncItemContents
            Just ts ->
                object ["contents" .= newSyncItemContents, "timestamp" .= ts]

instance ToSample NewSyncItem

data SyncResponse = SyncResponse
    { syncResponseAddedItems :: [ItemInfo TypedItem]
    , syncResponseNewRemoteItems :: [ItemInfo TypedItem]
    , syncResponseItemsToBeDeletedLocally :: [ItemUUID]
    } deriving (Show, Eq, Ord, Generic)

instance Validity SyncResponse where
    validate SyncResponse {..} =
        mconcat
            [ annotate syncResponseAddedItems "syncResponseAddedItems"
            , annotate syncResponseNewRemoteItems "syncResponseNewRemoteItems"
            , annotate
                  syncResponseItemsToBeDeletedLocally
                  "syncResponseItemsToBeDeletedLocally"
            , check (distinct syncResponseAddedItems) "Added items are distinct"
            , check
                  (distinct syncResponseNewRemoteItems)
                  "new items are distinct"
            , check
                  (distinct syncResponseItemsToBeDeletedLocally)
                  "deleted items are distinct"
            ]

instance FromJSON SyncResponse where
    parseJSON =
        withObject "SyncResponse" $ \o ->
            SyncResponse <$> o .: "added" <*> o .: "new" <*> o .: "deleted"

instance ToJSON SyncResponse where
    toJSON SyncResponse {..} =
        object
            [ "added" .= syncResponseAddedItems
            , "new" .= syncResponseNewRemoteItems
            , "deleted" .= syncResponseItemsToBeDeletedLocally
            ]

instance ToSample SyncResponse

distinct :: Eq a => [a] -> Bool
distinct ls = length ls == length (nub ls)
