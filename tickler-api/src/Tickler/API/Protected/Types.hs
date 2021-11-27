{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Tickler.API.Protected.Types
  ( module Tickler.API.Protected.Types,
    module Tickler.API.Types,
    module Tickler.Data,
    module Data.UUID.Typed,
  )
where

import Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Mergeful as Mergeful
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API ()
import qualified Intray.Data as Intray
import Tickler.API.Types
import Tickler.Data
import Web.HttpApiData

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
  { itemType :: !ItemType,
    itemData :: !ByteString
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity TypedItem

instance FromJSON TypedItem where
  parseJSON v =
    case v of
      Object o ->
        TypedItem <$> o .: "type"
          <*> ( do
                  d <- o .: "data"
                  case Base64.decode $ SB8.pack d of
                    Left err -> fail $ unwords ["Failed to decode base64-encoded typed item data:", err]
                    Right r -> pure r
              )
      String t -> pure $ TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}
      _ -> fail "Invalid json object for 'TypedItem'"

instance ToJSON TypedItem where
  toJSON TypedItem {..} =
    case itemType of
      TextItem ->
        case TE.decodeUtf8' itemData of
          Left _ -> object ["type" .= itemType, "data" .= SB8.unpack (Base64.encode itemData)]
          Right t -> JSON.String t
      _ -> object ["type" .= itemType, "data" .= SB8.unpack (Base64.encode itemData)]

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}

typedItemCase :: TypedItem -> Either String TypedItemCase
typedItemCase TypedItem {..} =
  case itemType of
    TextItem -> left show $ CaseTextItem <$> TE.decodeUtf8' itemData

newtype TypedItemCase
  = CaseTextItem Text
  deriving (Show, Read, Eq, Ord, Generic)

data AddedItem a = AddedItem
  { addedItemContents :: a,
    addedItemCreated :: UTCTime
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (AddedItem a)

instance ToJSON a => ToJSON (AddedItem a) where
  toJSON AddedItem {..} = object ["contents" .= addedItemContents, "created" .= addedItemCreated]

instance FromJSON a => FromJSON (AddedItem a) where
  parseJSON = withObject "AddedItem" $ \o -> AddedItem <$> o .: "contents" <*> o .: "created"

data Tickle a = Tickle
  { tickleContent :: !a,
    tickleScheduledDay :: !Day,
    tickleScheduledTime :: !(Maybe TimeOfDay),
    tickleRecurrence :: !(Maybe Recurrence)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (Tickle a)

instance FromJSON a => FromJSON (Tickle a) where
  parseJSON =
    withObject "Tickle" $ \o ->
      Tickle <$> o .: "content" <*> o .: "scheduled-day" <*> o .: "scheduled-time"
        <*> o
        .:? "recurrence"

instance ToJSON a => ToJSON (Tickle a) where
  toJSON Tickle {..} =
    object
      [ "content" .= tickleContent,
        "scheduled-day" .= tickleScheduledDay,
        "scheduled-time" .= tickleScheduledTime,
        "recurrence" .= tickleRecurrence
      ]

type TypedTickle = Tickle TypedItem

data ItemInfo a = ItemInfo
  { itemInfoIdentifier :: !ItemUUID,
    itemInfoContents :: !(Tickle a),
    itemInfoCreated :: !UTCTime,
    itemInfoTriggered :: !(Maybe TriggeredInfo)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (ItemInfo a)

instance ToJSON a => ToJSON (ItemInfo a) where
  toJSON ItemInfo {..} =
    object
      [ "id" .= itemInfoIdentifier,
        "contents" .= itemInfoContents,
        "created" .= itemInfoCreated,
        "triggered" .= itemInfoTriggered
      ]

instance FromJSON a => FromJSON (ItemInfo a) where
  parseJSON =
    withObject "ItemInfo TypedItem" $ \o ->
      ItemInfo <$> o .: "id" <*> o .: "contents" <*> o .: "created" <*> o .: "triggered"

type TypedItemInfo = ItemInfo TypedItem

data TriggeredInfo = TriggeredInfo
  { triggeredInfoTriggered :: !UTCTime,
    triggeredInfoTriggerTriggerAttempts :: ![TriggerAttempt]
  }
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity TriggeredInfo

instance FromJSON TriggeredInfo

instance ToJSON TriggeredInfo

data TriggerAttempt
  = EmailTriggerAttempt !TriggerUUID !EmailTriggerResult
  | IntrayTriggerAttempt !TriggerUUID !IntrayTriggerResult
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity TriggerAttempt

instance FromJSON TriggerAttempt

instance ToJSON TriggerAttempt

data EmailTriggerResult
  = EmailResultSent
  | EmailResultError !Text
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity EmailTriggerResult

instance FromJSON EmailTriggerResult

instance ToJSON EmailTriggerResult

instance ToHttpApiData EmailVerificationKey where
  toUrlPiece = emailVerificationKeyText

instance FromHttpApiData EmailVerificationKey where
  parseUrlPiece t =
    case parseEmailVerificationKeyText t of
      Nothing -> Left "Invalid email verification key"
      Just evk -> pure evk

data IntrayTriggerResult
  = IntrayAdditionSuccess !Intray.ItemUUID
  | IntrayAdditionFailure !Text
  deriving (Show, Read, Eq, Ord, Generic)

instance Validity IntrayTriggerResult

instance FromJSON IntrayTriggerResult

instance ToJSON IntrayTriggerResult

type AddItem = TypedTickle

data TriggerInfo a = TriggerInfo
  { triggerInfoIdentifier :: !TriggerUUID,
    triggerInfo :: !a
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity a => Validity (TriggerInfo a)

instance FromJSON a => FromJSON (TriggerInfo a) where
  parseJSON = withObject "TriggerInfo" $ \o -> TriggerInfo <$> o .: "uuid" <*> o .: "info"

instance ToJSON a => ToJSON (TriggerInfo a) where
  toJSON TriggerInfo {..} = object ["uuid" .= triggerInfoIdentifier, "info" .= triggerInfo]

instance Functor TriggerInfo where
  fmap f ti = ti {triggerInfo = f $ triggerInfo ti}

data SyncRequest = SyncRequest
  { syncRequestTickles :: !(Mergeful.SyncRequest Mergeful.ClientId ItemUUID (AddedItem TypedTickle))
  }
  deriving (Show, Eq, Generic)

instance Validity SyncRequest

instance FromJSON SyncRequest

instance ToJSON SyncRequest

data SyncResponse = SyncResponse
  { syncResponseTickles :: !(Mergeful.SyncResponse Mergeful.ClientId ItemUUID (AddedItem TypedTickle))
  }
  deriving (Show, Eq, Generic)

instance Validity SyncResponse

instance FromJSON SyncResponse

instance ToJSON SyncResponse

decodeTriggerInfo ::
  FromJSON a => TriggerType -> TriggerInfo TypedTriggerInfo -> Maybe (TriggerInfo a)
decodeTriggerInfo tt ti = unwrap $ decodeTypedTriggerInfo tt <$> ti
  where
    unwrap :: TriggerInfo (Maybe a) -> Maybe (TriggerInfo a)
    unwrap tmi =
      case triggerInfo tmi of
        Nothing -> Nothing
        Just i ->
          Just $ TriggerInfo {triggerInfoIdentifier = triggerInfoIdentifier tmi, triggerInfo = i}

data TypedTriggerInfo = TypedTriggerInfo
  { typedTriggerInfoType :: !TriggerType,
    typedTriggerInfoValue :: !JSON.Value
  }
  deriving (Show, Eq, Generic)

instance Validity TypedTriggerInfo

instance FromJSON TypedTriggerInfo

instance ToJSON TypedTriggerInfo

decodeTypedTriggerInfo :: FromJSON a => TriggerType -> TypedTriggerInfo -> Maybe a
decodeTypedTriggerInfo expectedType TypedTriggerInfo {..} =
  if typedTriggerInfoType == expectedType
    then case fromJSON typedTriggerInfoValue of
      JSON.Error _ -> Nothing
      Success ti -> Just ti
    else Nothing

data IntrayTriggerInfo = IntrayTriggerInfo
  { intrayTriggerInfoUrl :: !BaseUrl
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity IntrayTriggerInfo

instance FromJSON IntrayTriggerInfo

instance ToJSON IntrayTriggerInfo

data EmailTriggerInfo = EmailTriggerInfo
  { emailTriggerInfoEmailAddress :: !EmailAddress,
    emailTriggerInfoVerified :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity EmailTriggerInfo

instance FromJSON EmailTriggerInfo

instance ToJSON EmailTriggerInfo

data AddIntrayTrigger = AddIntrayTrigger
  { addIntrayTriggerUrl :: !BaseUrl,
    addIntrayTriggerUsername :: !Intray.Username,
    addIntrayTriggerAccessKey :: !Intray.AccessKeySecret
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AddIntrayTrigger

instance FromJSON AddIntrayTrigger

instance ToJSON AddIntrayTrigger

data AddEmailTrigger = AddEmailTrigger
  { addEmailTrigger :: !EmailAddress
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AddEmailTrigger

instance FromJSON AddEmailTrigger

instance ToJSON AddEmailTrigger
