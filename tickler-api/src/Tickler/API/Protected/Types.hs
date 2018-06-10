{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Protected.Types
    ( ItemFilter(..)
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , AddItem(..)
    , Added(..)
    , Synced(..)
    , SyncRequest(..)
    , SyncResponse(..)
    , TriggerType(..)
    , TriggerInfo(..)
    , decodeTriggerInfo
    , TypedTriggerInfo(..)
    , decodeTypedTriggerInfo
    , IntrayTriggerInfo(..)
    , EmailTriggerInfo(..)
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

import System.IO.Unsafe as Unsafe
import Data.Aeson as JSON
import qualified Data.Aeson as JSON (Result(Error))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import Data.List (nub)
import Data.Mergeless
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed

import Servant.Docs
import Web.HttpApiData

import qualified Intray.Data as Intray

import Intray.API ()

import Tickler.Data

import Tickler.API.Types

data ItemFilter
    = OnlyUntriggered
    | OnlyTriggered
    deriving (Show, Read, Eq, Enum, Bounded, Generic)

instance Validity ItemFilter

instance ToHttpApiData ItemFilter where
    toQueryParam = showTextData

instance FromHttpApiData ItemFilter where
    parseQueryParam = parseBoundedTextData

data TypedItem = TypedItem
    { itemType :: ItemType
    , itemData :: ByteString
    , itemScheduled :: UTCTime
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
                    Right r -> pure r) <*>
            o .: "scheduled"

instance ToJSON TypedItem where
    toJSON TypedItem {..} =
        object
            [ "type" .= itemType
            , "data" .= SB8.unpack (Base64.encode itemData)
            , "scheduled" .= itemScheduled
            ]

instance ToSample TypedItem where
    toSamples Proxy =
        singleSample $
        TypedItem
            TextItem
            "Hello World!"
            (Unsafe.unsafePerformIO getCurrentTime)

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

data TriggerInfo a = TriggerInfo
    { triggerInfoIdentifier :: TriggerUUID
    , triggerInfo :: a
    } deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (TriggerInfo a)

instance FromJSON a => FromJSON (TriggerInfo a) where
    parseJSON =
        withObject "TriggerInfo" $ \o ->
            TriggerInfo <$> o .: "uuid" <*> o .: "info"

instance ToJSON a => ToJSON (TriggerInfo a) where
    toJSON TriggerInfo {..} =
        object ["uuid" .= triggerInfoIdentifier, "info" .= triggerInfo]

instance ToSample a => ToSample (TriggerInfo a)

instance Functor TriggerInfo where
    fmap f ti = ti {triggerInfo = f $ triggerInfo ti}

instance ToSample (SyncRequest ItemUUID TypedItem)

instance ToSample (SyncResponse ItemUUID TypedItem)

decodeTriggerInfo ::
       FromJSON a
    => TriggerType
    -> TriggerInfo TypedTriggerInfo
    -> Maybe (TriggerInfo a)
decodeTriggerInfo tt ti = unwrap $ decodeTypedTriggerInfo tt <$> ti
  where
    unwrap :: TriggerInfo (Maybe a) -> Maybe (TriggerInfo a)
    unwrap tmi =
        case triggerInfo tmi of
            Nothing -> Nothing
            Just i ->
                Just $
                TriggerInfo
                    { triggerInfoIdentifier = triggerInfoIdentifier tmi
                    , triggerInfo = i
                    }

data TypedTriggerInfo = TypedTriggerInfo
    { typedTriggerInfoType :: TriggerType
    , typedTriggerInfoValue :: JSON.Value
    } deriving (Show, Eq, Generic)

instance Validity TypedTriggerInfo

instance FromJSON TypedTriggerInfo

instance ToJSON TypedTriggerInfo

instance ToSample TypedTriggerInfo

decodeTypedTriggerInfo ::
       FromJSON a => TriggerType -> TypedTriggerInfo -> Maybe a
decodeTypedTriggerInfo expectedType TypedTriggerInfo {..} =
    if typedTriggerInfoType == expectedType
        then case fromJSON typedTriggerInfoValue of
                 JSON.Error _ -> Nothing
                 Success ti -> Just ti
        else Nothing

data IntrayTriggerInfo = IntrayTriggerInfo
    { intrayTriggerInfoUrl :: BaseUrl
    } deriving (Show, Eq, Ord, Generic)

instance Validity IntrayTriggerInfo

instance FromJSON IntrayTriggerInfo

instance ToJSON IntrayTriggerInfo

instance ToSample IntrayTriggerInfo

data EmailTriggerInfo = EmailTriggerInfo
    { emailTriggerInfoEmailAddress :: EmailAddress
    } deriving (Show, Eq, Ord, Generic)

instance Validity EmailTriggerInfo

instance FromJSON EmailTriggerInfo

instance ToJSON EmailTriggerInfo

instance ToSample EmailTriggerInfo

data AddIntrayTrigger = AddIntrayTrigger
    { addIntrayTriggerUrl :: BaseUrl
    , addIntrayTriggerUsername :: Intray.Username
    , addIntrayTriggerAccessKey :: Intray.AccessKeySecret
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
