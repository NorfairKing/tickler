{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.API.Account.Types
    ( AccountInfo(..)
    , AccountUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    , module Data.UUID.Typed
    ) where

import Import

import Data.Aeson as JSON
import Data.Time
import Data.UUID.Typed

import Servant.Docs

import Tickler.Data

import Tickler.API.Types ()

data AccountInfo = AccountInfo
    { accountInfoUUID :: AccountUUID
    , accountInfoUsername :: Username
    , accountInfoCreatedTimestamp :: UTCTime
    , accountInfoLastLogin :: Maybe UTCTime
    , accountInfoAdmin :: Bool
    } deriving (Show, Eq, Ord, Generic)

instance Validity AccountInfo

instance FromJSON AccountInfo where
    parseJSON =
        withObject "AccountInfo" $ \o ->
            AccountInfo <$> o .: "uuid" <*> o .: "username" <*> o .: "created" <*>
            o .: "last-login" <*>
            o .: "admin"

instance ToJSON AccountInfo where
    toJSON AccountInfo {..} =
        object
            [ "uuid" .= accountInfoUUID
            , "username" .= accountInfoUsername
            , "created" .= accountInfoCreatedTimestamp
            , "last-login" .= accountInfoLastLogin
            , "admin" .= accountInfoAdmin
            ]

instance ToSample AccountInfo
