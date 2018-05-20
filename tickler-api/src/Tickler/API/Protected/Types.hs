{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.API.Protected.Types
    ( ItemFilter(..)
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , AddItem(..)
    , SyncRequest(..)
    , NewSyncItem(..)
    , SyncResponse(..)
    , TriggerInfo(..)
    , AddIntrayTrigger(..)
    , AddEmailTrigger(..)
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
import Web.HttpApiData

import Tickler.Data

import Tickler.API.Types

data ItemFilter
    = OnlyUntriggered
    | OnlyTriggered
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance ToHttpApiData ItemFilter where
    toQueryParam = showTextData

instance FromHttpApiData ItemFilter where
    parseQueryParam = parseBoundedTextData

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
    , itemInfoCreated :: UTCTime
    , itemInfoScheduled :: UTCTime
    , itemInfoTriggered :: Bool
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (ItemInfo a)

instance ToJSON a => ToJSON (ItemInfo a) where
    toJSON ItemInfo {..} =
        object
            [ "id" .= itemInfoIdentifier
            , "contents" .= itemInfoContents
            , "created" .= itemInfoCreated
            , "scheduled" .= itemInfoScheduled
            , "triggered" .= itemInfoTriggered
            ]

instance FromJSON a => FromJSON (ItemInfo a) where
    parseJSON =
        withObject "ItemInfo TypedItem" $ \o ->
            ItemInfo <$> o .: "id" <*> o .: "contents" <*> o .: "created" <*>
            o .: "scheduled" <*>
            o .: "triggered"

instance ToSample a => ToSample (ItemInfo a)

data AddItem = AddItem
    { addItemTypedItem :: TypedItem
    , addItemScheduled :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity AddItem

instance FromJSON AddItem where
    parseJSON =
        withObject "AddItem" $ \o ->
            AddItem <$> o .: "item" <*> o .: "scheduled"

instance ToJSON AddItem where
    toJSON AddItem {..} =
        object ["item" .= addItemTypedItem, "scheduled" .= addItemScheduled]

instance ToSample AddItem

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
    , newSyncItemCreated :: Maybe UTCTime
    , newSyncItemScheduled :: UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity NewSyncItem

instance FromJSON NewSyncItem where
    parseJSON =
        withObject
            "NewSyncItem"
            (\o ->
                 NewSyncItem <$> o .: "contents" <*> o .:? "created" <*>
                 o .: "scheduled")

instance ToJSON NewSyncItem where
    toJSON NewSyncItem {..} =
        object
            [ "contents" .= newSyncItemContents
            , "created" .= newSyncItemCreated
            , "scheduled" .= newSyncItemScheduled
            ]

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

data TriggerInfo = TriggerInfo
    { triggerIdentifier :: TriggerUUID
    } deriving (Show, Eq, Ord, Generic)

instance Validity TriggerInfo

instance FromJSON TriggerInfo

instance ToJSON TriggerInfo

instance ToSample TriggerInfo

data AddIntrayTrigger = AddIntrayTrigger
    { addIntrayTriggerUrl :: BaseUrl
    } deriving (Show, Eq, Ord, Generic)

instance Validity AddIntrayTrigger

instance FromJSON AddIntrayTrigger

instance ToJSON AddIntrayTrigger

instance ToSample AddIntrayTrigger

data AddEmailTrigger = AddEmailTrigger
    { addEmailTrigger :: EmailAddress
    } deriving (Show, Eq, Ord, Generic)

instance Validity AddEmailTrigger

instance FromJSON AddEmailTrigger

instance ToJSON AddEmailTrigger

instance ToSample AddEmailTrigger

distinct :: Eq a => [a] -> Bool
distinct ls = length ls == length (nub ls)
