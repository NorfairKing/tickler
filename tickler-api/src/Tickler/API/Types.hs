{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Types
    ( ProtectAPI
    , AuthCookie(..)
    , Registration(..)
    , LoginForm(..)
    , LoopersInfo(..)
    , LooperInfo(..)
    , LooperStatus(..)
    , GetDocsResponse(..)
    , HashedPassword
    , passwordHash
    , validatePassword
    , ItemUUID
    , AccountUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    , module Data.UUID.Typed
    ) where

import Import

import Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed

import Text.Blaze as HTML
import Text.Blaze.Html as HTML

import Servant.API
import Servant.Auth
import Servant.Auth.Docs ()
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client.Core
import Servant.Docs
import Servant.HTML.Blaze

import Intray.API ()

import Tickler.Data

type ProtectAPI = Auth '[ JWT] AuthCookie

newtype AuthCookie = AuthCookie
    { authCookieUserUUID :: AccountUUID
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromJWT AuthCookie

instance ToJWT AuthCookie

data Registration = Registration
    { registrationUsername :: Username
    , registrationPassword :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity Registration

instance ToJSON Registration where
    toJSON Registration {..} =
        object
            ["name" .= registrationUsername, "password" .= registrationPassword]

instance FromJSON Registration where
    parseJSON =
        withObject "Registration Text" $ \o ->
            Registration <$> o .: "name" <*> o .: "password"

instance ToSample Registration

data LoginForm = LoginForm
    { loginFormUsername :: Username
    , loginFormPassword :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
    parseJSON =
        withObject "LoginForm" $ \o ->
            LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
    toJSON LoginForm {..} =
        object
            ["username" .= loginFormUsername, "password" .= loginFormPassword]

instance ToSample LoginForm

instance ToSample Username

data LoopersInfo = LoopersInfo
    { emailerLooperInfo :: LooperInfo
    , triggererLooperInfo :: LooperInfo
    , verificationEmailConverterLooperInfo :: LooperInfo
    , triggeredIntrayItemSchedulerLooperInfo :: LooperInfo
    , triggeredIntrayItemSenderLooperInfo :: LooperInfo
    , triggeredEmailSchedulerLooperInfo :: LooperInfo
    , triggeredEmailConverterLooperInfo :: LooperInfo
    } deriving (Show, Eq, Generic)

instance Validity LoopersInfo

instance FromJSON LoopersInfo

instance ToJSON LoopersInfo

instance ToSample LoopersInfo

data LooperInfo = LooperInfo
    { looperInfoStatus :: LooperStatus
    , looperInfoPeriod :: Maybe Int
    , looperInfoRetryDelay :: Maybe Int
    , looperInfoRetryAmount :: Maybe Int
    } deriving (Show, Eq, Generic)

instance Validity LooperInfo

instance FromJSON LooperInfo

instance ToJSON LooperInfo

instance ToSample LooperInfo

data LooperStatus
    = LooperStatusDisabled
    | LooperStatusRunning
    | LooperStatusErrored Text
    | LooperStatusStopped
    deriving (Show, Eq, Generic)

instance Validity LooperStatus

instance FromJSON LooperStatus

instance ToJSON LooperStatus

instance ToSample LooperStatus

newtype GetDocsResponse = GetDocsResponse
    { unGetDocsResponse :: HTML.Html
    } deriving (Generic)

instance MimeUnrender HTML GetDocsResponse where
    mimeUnrender Proxy bs =
        Right $ GetDocsResponse $ HTML.unsafeLazyByteString bs

instance ToSample GetDocsResponse where
    toSamples Proxy = singleSample $ GetDocsResponse "Documentation (In HTML)."

instance ToMarkup GetDocsResponse where
    toMarkup (GetDocsResponse html) = toMarkup html

instance ToSample TimeZone where
    toSamples Proxy = singleSample utc

instance ToSample BaseUrl where
    toSamples Proxy = singleSample $ BaseUrl Https "tickler.cs-syd.eu" 8000 ""

instance ToSample EmailAddress where
    toSamples Proxy = singleSample $ unsafeEmailAddress "user" "example.com"

instance ToSample TriggerType

instance ToSample JSON.Value where
    toSamples Proxy =
        singleSample $
        object ["Here" .= ("Be" :: Text), "A" .= ("Value" :: Text)]
