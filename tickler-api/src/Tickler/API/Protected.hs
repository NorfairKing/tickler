{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Tickler.API.Protected where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Data.Time
import Import
import Intray.API ()
import qualified Intray.Data as Intray
import Servant.API
import Servant.Auth
import Servant.Auth.Server
import Tickler.API.Account
import Tickler.Data

type TicklerProtectedAPI = ToServantApi TicklerProtectedSite

type ProtectAPI = Auth '[JWT] AuthCookie

newtype AuthCookie = AuthCookie
  { authCookieUserUUID :: AccountUUID
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AuthCookie)

instance FromJWT AuthCookie

instance ToJWT AuthCookie

instance HasCodec AuthCookie where
  codec =
    object "AuthCookie" $
      AuthCookie
        <$> requiredField "id" "identifier" .= authCookieUserUUID

data TicklerProtectedSite route = TicklerProtectedSite
  { getItems :: !(route :- GetItems),
    postItem :: !(route :- PostItem),
    getItem :: !(route :- GetItem),
    putItem :: !(route :- PutItem),
    deleteItem :: !(route :- DeleteItem),
    getTriggers :: !(route :- GetTriggers),
    getTrigger :: !(route :- GetTrigger),
    postIntrayTrigger :: !(route :- PostIntrayTrigger),
    postEmailTrigger :: !(route :- PostEmailTrigger),
    postEmailTriggerVerify :: !(route :- PostEmailTriggerVerify),
    postEmailTriggerResendVerificationEmail :: !(route :- PostEmailTriggerResendVerificationEmail),
    deleteTrigger :: !(route :- DeleteTrigger),
    getAccountInfo :: !(route :- GetAccountInfo),
    getAccountSettings :: !(route :- GetAccountSettings),
    postChangePassphrase :: route :- PostChangePassphrase,
    putAccountSettings :: !(route :- PutAccountSettings),
    deleteAccount :: !(route :- DeleteAccount),
    postInitiateStripeCheckoutSession :: !(route :- PostInitiateStripeCheckoutSession)
  }
  deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type GetItems =
  ProtectAPI
    :> "item"
    :> Get '[JSON] [ItemInfo]

type GetItem =
  ProtectAPI
    :> "item"
    :> Capture "id" ItemUUID
    :> Get '[JSON] ItemInfo

data ItemInfo = ItemInfo
  { itemInfoIdentifier :: !ItemUUID,
    itemInfoContents :: !Tickle,
    itemInfoCreated :: !UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ItemInfo)

instance Validity ItemInfo

instance HasCodec ItemInfo where
  codec =
    object "ItemInfo" $
      ItemInfo
        <$> requiredField "id" "identifier" .= itemInfoIdentifier
        <*> requiredField "contents" "contents" .= itemInfoContents
        <*> requiredField "created" "created timestam" .= itemInfoCreated

type PostItem =
  ProtectAPI
    :> "item"
    :> ReqBody '[JSON] Tickle
    :> Post '[JSON] ItemUUID

type PutItem =
  ProtectAPI
    :> "item"
    :> Capture "id" ItemUUID
    :> ReqBody '[JSON] Tickle
    :> Put '[JSON] NoContent

data Tickle = Tickle
  { tickleContent :: !Text,
    tickleScheduledDay :: !Day,
    tickleScheduledTime :: !(Maybe TimeOfDay),
    tickleRecurrence :: !(Maybe Recurrence)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Tickle)

instance Validity Tickle where
  validate t@Tickle {..} =
    mconcat
      [ genericValidate t,
        declare "the content is not empty" $ not $ T.null tickleContent
      ]

instance HasCodec Tickle where
  codec =
    object "Tickle" $
      Tickle
        <$> requiredField "content" "content" .= tickleContent
        <*> requiredField "scheduled-day" "scheduled day" .= tickleScheduledDay
        <*> optionalField "scheduled-time" "scheduled time of day" .= tickleScheduledTime
        <*> optionalField "recurrence" "recurrence" .= tickleRecurrence

type DeleteItem =
  ProtectAPI
    :> "item"
    :> Capture "id" ItemUUID
    :> Delete '[JSON] NoContent

type GetTriggers =
  ProtectAPI
    :> "trigger"
    :> Get '[JSON] [TriggerInfo]

type GetTrigger =
  ProtectAPI
    :> "trigger"
    :> Capture "id" TriggerUUID
    :> Get '[JSON] TriggerInfo

data TriggerInfo = TriggerInfo
  { triggerInfoIdentifier :: !TriggerUUID,
    triggerInfo :: !Trigger
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TriggerInfo)

instance Validity TriggerInfo

instance HasCodec TriggerInfo where
  codec =
    object "TriggerInfo" $
      TriggerInfo
        <$> requiredField "id" "identifier" .= triggerInfoIdentifier
        <*> requiredField "trigger" "trigger" .= triggerInfo

data Trigger
  = TriggerIntray IntrayTriggerInfo
  | TriggerEmail EmailTriggerInfo
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Trigger)

instance Validity Trigger

instance HasCodec Trigger where
  codec = dimapCodec f g $ eitherCodec codec codec
    where
      f = \case
        Left it -> TriggerIntray it
        Right et -> TriggerEmail et

      g = \case
        TriggerIntray it -> Left it
        TriggerEmail et -> Right et

data IntrayTriggerInfo = IntrayTriggerInfo
  { intrayTriggerInfoUrl :: !BaseUrl
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec IntrayTriggerInfo)

instance Validity IntrayTriggerInfo

instance HasCodec IntrayTriggerInfo where
  codec =
    object "IntrayTriggerInfo" $
      IntrayTriggerInfo
        <$> requiredFieldWith "url" (codecViaAeson "BaseUrl") "base url of the intray instance" .= intrayTriggerInfoUrl

data EmailTriggerInfo = EmailTriggerInfo
  { emailTriggerInfoEmailAddress :: !EmailAddress,
    emailTriggerInfoVerified :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec EmailTriggerInfo)

instance Validity EmailTriggerInfo

instance HasCodec EmailTriggerInfo where
  codec =
    object "EmailTriggerInfo" $
      EmailTriggerInfo
        <$> requiredField "email-address" "email address" .= emailTriggerInfoEmailAddress
        <*> requiredField "verified" "verified" .= emailTriggerInfoVerified

type PostIntrayTrigger =
  ProtectAPI
    :> "trigger"
    :> "intray"
    :> ReqBody '[JSON] AddIntrayTrigger
    :> Post '[JSON] (Either Text TriggerUUID)

data AddIntrayTrigger = AddIntrayTrigger
  { addIntrayTriggerUrl :: !BaseUrl,
    addIntrayTriggerUsername :: !Intray.Username,
    addIntrayTriggerAccessKey :: !Intray.AccessKeySecret
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AddIntrayTrigger)

instance Validity AddIntrayTrigger

instance HasCodec AddIntrayTrigger where
  codec =
    object "AddIntrayTrigger" $
      AddIntrayTrigger
        <$> requiredFieldWith "url" (codecViaAeson "BaseUrl") "base url of the intray instance" .= addIntrayTriggerUrl
        <*> requiredField "username" "username at that intray instance" .= addIntrayTriggerUsername
        <*> requiredField "access key secret" "access key to log into intray" .= addIntrayTriggerAccessKey

type PostEmailTrigger =
  ProtectAPI
    :> "trigger"
    :> "email"
    :> ReqBody '[JSON] AddEmailTrigger
    :> Post '[JSON] TriggerUUID

data AddEmailTrigger = AddEmailTrigger
  { addEmailTriggerEmailAddress :: !EmailAddress
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AddEmailTrigger)

instance Validity AddEmailTrigger

instance HasCodec AddEmailTrigger where
  codec =
    object "AddEmailTrigger" $
      AddEmailTrigger
        <$> requiredField "email-address" "email address" .= addEmailTriggerEmailAddress

type PostEmailTriggerVerify =
  ProtectAPI
    :> "trigger"
    :> "email"
    :> Capture "id" TriggerUUID
    :> "verify"
    :> Capture "key" EmailVerificationKey
    :> Verb 'POST 204 '[JSON] NoContent

instance ToHttpApiData EmailVerificationKey where
  toUrlPiece = emailVerificationKeyText

instance FromHttpApiData EmailVerificationKey where
  parseUrlPiece t =
    case parseEmailVerificationKeyText t of
      Nothing -> Left "Invalid email verification key"
      Just evk -> pure evk

type PostEmailTriggerResendVerificationEmail =
  ProtectAPI
    :> "trigger"
    :> "email"
    :> Capture "id" TriggerUUID
    :> "resend"
    :> Verb 'POST 204 '[JSON] NoContent

type DeleteTrigger =
  ProtectAPI
    :> "trigger"
    :> "delete"
    :> Capture "id" TriggerUUID
    :> Delete '[JSON] NoContent

type GetAccountInfo =
  ProtectAPI
    :> "account"
    :> Get '[JSON] AccountInfo

type GetAccountSettings =
  ProtectAPI
    :> "account"
    :> "settings"
    :> Get '[JSON] AccountSettings

type PostChangePassphrase =
  ProtectAPI
    :> "account"
    :> "change-passphrase"
    :> ReqBody '[JSON] ChangePassphrase
    :> Verb 'POST 204 '[JSON] NoContent

data ChangePassphrase = ChangePassphrase
  { changePassphraseOld :: Text,
    changePassphraseNew :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ChangePassphrase)

instance Validity ChangePassphrase

instance HasCodec ChangePassphrase where
  codec =
    object "ChangePassphrase" $
      ChangePassphrase
        <$> requiredField "old-passphrase" "old passphrase" .= changePassphraseOld
        <*> requiredField "new-passphrase" "new passphrase" .= changePassphraseNew

type PutAccountSettings =
  ProtectAPI
    :> "account"
    :> "settings"
    :> ReqBody '[JSON] AccountSettings
    :> Put '[JSON] NoContent

type DeleteAccount =
  ProtectAPI
    :> "account"
    :> Delete '[JSON] NoContent

type PostInitiateStripeCheckoutSession =
  ProtectAPI
    :> "checkout"
    :> "stripe"
    :> "session"
    :> ReqBody '[JSON] InitiateStripeCheckoutSession
    :> Post '[JSON] InitiatedCheckoutSession

data InitiateStripeCheckoutSession = InitiateStripeCheckoutSession
  { initiateStripeCheckoutSessionSuccessUrl :: Text,
    initiateStripeCheckoutSessionCanceledUrl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec InitiateStripeCheckoutSession)

instance Validity InitiateStripeCheckoutSession

instance HasCodec InitiateStripeCheckoutSession where
  codec =
    object "InitiateStripeCheckoutSession" $
      InitiateStripeCheckoutSession
        <$> requiredField "success" "success url" .= initiateStripeCheckoutSessionSuccessUrl
        <*> requiredField "canceled" "canceled url" .= initiateStripeCheckoutSessionCanceledUrl

data InitiatedCheckoutSession = InitiatedCheckoutSession
  { initiatedCheckoutSessionId :: Text,
    initiatedCheckoutSessionCustomerId :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec InitiatedCheckoutSession)

instance Validity InitiatedCheckoutSession

instance HasCodec InitiatedCheckoutSession where
  codec =
    object "InitiatedCheckoutSession" $
      InitiatedCheckoutSession
        <$> requiredField "session" "session identifier" .= initiatedCheckoutSessionId
        <*> requiredField "customer" "customer identifier" .= initiatedCheckoutSessionCustomerId
