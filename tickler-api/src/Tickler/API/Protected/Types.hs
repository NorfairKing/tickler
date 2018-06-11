{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Tickler.API.Protected.Types
    ( ItemFilter(..)
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , Tickle(..)
    , TypedTickle
    , ItemInfo(..)
    , TypedItemInfo
    , AddItem
    , Mergeless.Added(..)
    , Mergeless.Synced(..)
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

import Data.Aeson as JSON
import qualified Data.Aeson as JSON (Result(Error))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Mergeless as Mergeless
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
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity TypedItem

instance FromJSON TypedItem where
    parseJSON v =
        (withObject "TypedItem" $ \o ->
             TypedItem <$> o .: "type" <*>
             (do d <- o .: "data"
                 case Base64.decode $ SB8.pack d of
                     Left err ->
                         fail $
                         unwords
                             [ "Failed to decode base64-encoded typed item data:"
                             , err
                             ]
                     Right r -> pure r))
            v <|>
        (withText "TypedItem" $ \t ->
             pure $ TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t})
            v

instance ToJSON TypedItem where
    toJSON TypedItem {..} =
        case itemType of
            TextItem ->
                case TE.decodeUtf8' itemData of
                    Left _ ->
                        object
                            [ "type" .= itemType
                            , "data" .= SB8.unpack (Base64.encode itemData)
                            ]
                    Right t -> JSON.String t
            _ ->
                object
                    [ "type" .= itemType
                    , "data" .= SB8.unpack (Base64.encode itemData)
                    ]

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

data Tickle a = Tickle
    { tickleContent :: a
    , tickleScheduled :: UTCTime
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (Tickle a)


instance FromJSON a => FromJSON (Tickle a) where
    parseJSON =
        withObject "Tickle" $ \o ->
            Tickle <$> o .: "content" <*> o .: "scheduled"

instance ToJSON a => ToJSON (Tickle a) where
    toJSON Tickle {..} =
        object ["content" .= tickleContent, "scheduled" .= tickleScheduled]
instance ToSample a => ToSample (Tickle a)

type TypedTickle = Tickle TypedItem

data ItemInfo a = ItemInfo
    { itemInfoIdentifier :: ItemUUID
    , itemInfoContents :: Tickle a
    , itemInfoCreated :: UTCTime
    , itemInfoSynced :: UTCTime
    , itemInfoTriggered :: Maybe UTCTime
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (ItemInfo a)

instance ToJSON a => ToJSON (ItemInfo a) where
    toJSON ItemInfo {..} =
        object
            [ "id" .= itemInfoIdentifier
            , "contents" .= itemInfoContents
            , "created" .= itemInfoCreated
            , "synced" .= itemInfoSynced
            , "triggered" .= itemInfoTriggered
            ]

instance FromJSON a => FromJSON (ItemInfo a) where
    parseJSON =
        withObject "ItemInfo TypedItem" $ \o ->
            ItemInfo <$> o .: "id" <*> o .: "contents" <*> o .: "created" <*>
            o .: "synced" <*>
            o .: "triggered"

instance ToSample a => ToSample (ItemInfo a)

type TypedItemInfo = ItemInfo TypedItem

type AddItem = TypedTickle

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

data SyncRequest = SyncRequest
    { syncRequestTickles :: Mergeless.SyncRequest ItemUUID TypedTickle
    } deriving (Show, Eq, Ord, Generic)

instance Validity SyncRequest

instance FromJSON SyncRequest

instance ToJSON SyncRequest

instance ToSample SyncRequest

instance ToSample (Mergeless.SyncRequest ItemUUID TypedTickle)

data SyncResponse = SyncResponse
    { syncResponseTickles :: Mergeless.SyncResponse ItemUUID TypedTickle
    } deriving (Show, Eq, Ord, Generic)

instance Validity SyncResponse

instance FromJSON SyncResponse

instance ToJSON SyncResponse

instance ToSample SyncResponse

instance ToSample (Mergeless.SyncResponse ItemUUID TypedTickle)

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
