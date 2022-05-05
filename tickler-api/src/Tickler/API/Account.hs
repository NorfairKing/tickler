{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.API.Account where

import Data.Aeson as JSON
import Data.Time
import Import
import Tickler.API.Types ()
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
  deriving (Show, Eq, Ord, Generic)

instance Validity AccountInfo

instance FromJSON AccountInfo where
  parseJSON =
    withObject "AccountInfo" $ \o ->
      AccountInfo <$> o .: "uuid" <*> o .: "username" <*> o .: "created" <*> o .: "last-login"
        <*> o .: "admin"
        <*> o .: "tickler-item-count"
        <*> o .: "triggered-item-count"
        <*> o .: "status"

instance ToJSON AccountInfo where
  toJSON AccountInfo {..} =
    object
      [ "uuid" .= accountInfoUUID,
        "username" .= accountInfoUsername,
        "created" .= accountInfoCreated,
        "last-login" .= accountInfoLastLogin,
        "admin" .= accountInfoAdmin,
        "tickler-item-count" .= accountInfoTicklerItemCount,
        "triggered-item-count" .= accountInfoTriggeredItemCount,
        "status" .= accountInfoStatus
      ]

data PaidStatus
  = HasNotPaid Int -- Number of extra items that they're still allowed
  | HasPaid UTCTime
  | NoPaymentNecessary
  deriving (Show, Eq, Ord, Generic)

instance Validity PaidStatus

instance FromJSON PaidStatus where
  parseJSON =
    withObject "PaidStatus" $ \o -> do
      t <- o .: "status"
      case (t :: Text) of
        "not-paid" -> HasNotPaid <$> o .: "items-left"
        "paid" -> HasPaid <$> o .: "until"
        "no-payment-necessary" -> pure NoPaymentNecessary
        _ -> fail "Unknown PaidStatus"

instance ToJSON PaidStatus where
  toJSON =
    let o t vs = object $ ("status" .= (t :: Text)) : vs
     in \case
          HasNotPaid itemsLeft -> o "not-paid" ["items-left" .= itemsLeft]
          HasPaid ut -> o "paid" ["until" .= ut]
          NoPaymentNecessary -> o "no-payment-necessary" []

newtype AccountSettings = AccountSettings
  { accountSettingsTimeZone :: TimeZone
  }
  deriving (Show, Eq, Ord, Generic)

instance Validity AccountSettings where
  validate (AccountSettings tz@TimeZone {..}) =
    mconcat [validate tz, decorateList timeZoneName validateCharNotUtf16SurrogateCodePoint]

instance FromJSON AccountSettings where
  parseJSON = withObject "AccountSettings" $ \o -> AccountSettings <$> o .: "timezone"

instance ToJSON AccountSettings where
  toJSON AccountSettings {..} = object ["timezone" .= accountSettingsTimeZone]
