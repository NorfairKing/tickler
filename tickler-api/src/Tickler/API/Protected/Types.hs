{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Text as T
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

instance Validity Tickle where
  validate t@Tickle {..} =
    mconcat
      [ genericValidate t,
        declare "the content is not empty" $ not $ T.null tickleContent
      ]

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

data TriggerInfo = TriggerInfo
  { triggerInfoIdentifier :: !TriggerUUID,
    triggerInfo :: !Trigger
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity TriggerInfo

instance FromJSON TriggerInfo where
  parseJSON = withObject "TriggerInfo" $ \o -> TriggerInfo <$> o .: "uuid" <*> o .: "info"

instance ToJSON TriggerInfo where
  toJSON TriggerInfo {..} = object ["uuid" .= triggerInfoIdentifier, "info" .= triggerInfo]

data Trigger
  = TriggerIntray IntrayTriggerInfo
  | TriggerEmail EmailTriggerInfo
  deriving (Show, Eq, Ord, Generic)

instance Validity Trigger

instance FromJSON Trigger where
  parseJSON v =
    TriggerIntray <$> parseJSON v
      <|> TriggerEmail <$> parseJSON v

instance ToJSON Trigger where
  toJSON = \case
    TriggerIntray iti -> toJSON iti
    TriggerEmail eti -> toJSON eti

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
