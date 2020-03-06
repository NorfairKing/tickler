{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.API.Account.Types
  ( module Tickler.API.Account.Types
  , module Data.UUID.Typed
  ) where

import Import

import Data.Aeson as JSON
import Data.Time
import Data.UUID.Typed

import Servant.Docs

import Tickler.Data

import Tickler.API.Types ()

data AccountInfo =
  AccountInfo
    { accountInfoUUID :: AccountUUID
    , accountInfoUsername :: Username
    , accountInfoCreated :: UTCTime
    , accountInfoLastLogin :: Maybe UTCTime
    , accountInfoAdmin :: Bool
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity AccountInfo

instance FromJSON AccountInfo where
  parseJSON =
    withObject "AccountInfo" $ \o ->
      AccountInfo <$> o .: "uuid" <*> o .: "username" <*> o .: "created" <*> o .: "last-login" <*>
      o .: "admin"

instance ToJSON AccountInfo where
  toJSON AccountInfo {..} =
    object
      [ "uuid" .= accountInfoUUID
      , "username" .= accountInfoUsername
      , "created" .= accountInfoCreated
      , "last-login" .= accountInfoLastLogin
      , "admin" .= accountInfoAdmin
      ]

instance ToSample AccountInfo

newtype AccountSettings =
  AccountSettings
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

instance ToSample AccountSettings
