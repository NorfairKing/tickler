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
import Data.Time
import Data.UUID.Typed
import Import
import Intray.API ()
import qualified Intray.Data as Intray
import Tickler.API.Types
import Tickler.Data
import Web.HttpApiData

data Tickle = Tickle
  { tickleContent :: !Text,
    tickleScheduledDay :: !Day,
    tickleScheduledTime :: !(Maybe TimeOfDay),
    tickleRecurrence :: !(Maybe Recurrence)
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity Tickle

instance FromJSON Tickle where
  parseJSON =
    withObject "Tickle" $ \o ->
      Tickle
        <$> o .: "content"
        <*> o .: "scheduled-day"
        <*> o .: "scheduled-time"
        <*> o .:? "recurrence"

instance ToJSON Tickle where
  toJSON Tickle {..} =
    object
      [ "content" .= tickleContent,
        "scheduled-day" .= tickleScheduledDay,
        "scheduled-time" .= tickleScheduledTime,
        "recurrence" .= tickleRecurrence
      ]

data ItemInfo = ItemInfo
  { itemInfoIdentifier :: !ItemUUID,
    itemInfoContents :: !Tickle,
    itemInfoCreated :: !UTCTime
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity ItemInfo

instance ToJSON ItemInfo where
  toJSON ItemInfo {..} =
    object
      [ "id" .= itemInfoIdentifier,
        "contents" .= itemInfoContents,
        "created" .= itemInfoCreated
      ]

instance FromJSON ItemInfo where
  parseJSON =
    withObject "ItemInfo TypedItem" $ \o ->
      ItemInfo
        <$> o .: "id"
        <*> o .: "contents"
        <*> o .: "created"

instance ToHttpApiData EmailVerificationKey where
  toUrlPiece = emailVerificationKeyText

instance FromHttpApiData EmailVerificationKey where
  parseUrlPiece t =
    case parseEmailVerificationKeyText t of
      Nothing -> Left "Invalid email verification key"
      Just evk -> pure evk

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
