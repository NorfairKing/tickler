{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.API.Account where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import Import
import Tickler.Data

data AccountInfo = AccountInfo
  { accountInfoUUID :: AccountUUID,
    accountInfoUsername :: Username,
    accountInfoCreated :: UTCTime,
    accountInfoLastLogin :: Maybe UTCTime,
    accountInfoAdmin :: Bool,
    accountInfoTicklerItemCount :: Int,
    accountInfoTriggeredItemCount :: Int,
    accountInfoStatus :: PaidStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AccountInfo)

instance Validity AccountInfo

instance HasCodec AccountInfo where
  codec =
    object "AccountInfo" $
      AccountInfo
        <$> requiredField "id" "identifier" .= accountInfoUUID
        <*> requiredField "username" "username" .= accountInfoUsername
        <*> requiredField "created" "created timestamp" .= accountInfoCreated
        <*> requiredField "last-login" "last login timestamp" .= accountInfoLastLogin
        <*> requiredField "admin" "whether this account is an admin" .= accountInfoAdmin
        <*> requiredField "tickler-item-count" "number of tickler items" .= accountInfoTicklerItemCount
        <*> requiredField "triggered-item-count" "number of triggered items" .= accountInfoTriggeredItemCount
        <*> requiredField "status" "paid status" .= accountInfoStatus

data PaidStatus
  = HasNotPaid Int -- Number of extra items that they're still allowed
  | HasPaid UTCTime
  | NoPaymentNecessary
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec PaidStatus)

instance Validity PaidStatus

instance HasCodec PaidStatus where
  codec =
    dimapCodec f g $
      eitherCodec hasNotPaidCodec $
        eitherCodec hasPaidCodec noPaymentNecessaryCodec
    where
      f = \case
        Left i -> HasNotPaid i
        Right (Left u) -> HasPaid u
        Right (Right ()) -> NoPaymentNecessary
      g = \case
        HasNotPaid i -> Left i
        HasPaid u -> Right (Left u)
        NoPaymentNecessary -> Right (Right ())
      hasNotPaidCodec :: JSONCodec Int
      hasNotPaidCodec = object "HasNotPaid" $ statusField "not-paid" id <*> requiredField "items-left" "number of free items left"
      hasPaidCodec :: JSONCodec UTCTime
      hasPaidCodec = object "HasPaid" $ statusField "paid" id <*> requiredField "until" "end of the subscription "
      noPaymentNecessaryCodec :: JSONCodec ()
      noPaymentNecessaryCodec = object "NoPaymentNecessary" $ statusField "no-payment-necessary" ()

statusField :: Text -> a -> ObjectCodec b a
statusField statusName a =
  a <$ requiredFieldWith' "status" (literalTextCodec statusName) .= const statusName

newtype AccountSettings = AccountSettings
  { accountSettingsTimeZone :: TimeZone
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AccountSettings)

instance Validity AccountSettings where
  validate (AccountSettings tz@TimeZone {..}) =
    mconcat [validate tz, decorateList timeZoneName validateCharNotUtf16SurrogateCodePoint]

instance HasCodec AccountSettings where
  codec =
    object "AccountSettings" $
      AccountSettings
        <$> requiredField "timezone" "timezone" .= accountSettingsTimeZone
