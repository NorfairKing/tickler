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
import qualified Data.UUID as UUID
import Data.UUID.Typed

import Text.Blaze as HTML
import Text.Blaze.Html as HTML
import Text.Pandoc as Pandoc

import Web.Cookie

import Servant.API
import Servant.Auth
import Servant.Auth.Docs ()
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client.Core
import Servant.Docs
import Servant.HTML.Blaze

import Tickler.Data

type ProtectAPI = Auth '[ JWT] AuthCookie

newtype AuthCookie = AuthCookie
    { authCookieUserUUID :: AccountUUID
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromJWT AuthCookie

instance ToJWT AuthCookie

instance ToSample UTCTime where
    toSamples Proxy = singleSample $ UTCTime (fromGregorian 2018 2 10) 42

instance ToSample Text where
    toSamples Proxy = singleSample "Example Text"

instance ToSample (UUID a) where
    toSamples Proxy = singleSample (UUID $ UUID.fromWords 0 0 0 0)

instance ToSample Int where
    toSamples Proxy = singleSample 42

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

instance ToSample SetCookie where
    toSamples Proxy = singleSample def

newtype GetDocsResponse = GetDocsResponse
    { unGetDocsResponse :: HTML.Html
    } deriving (Generic)

instance MimeUnrender HTML GetDocsResponse where
    mimeUnrender Proxy bs =
        left show $
        runPure $ do
            pandoc <- Pandoc.readHtml def $ TE.decodeUtf8 $ LB.toStrict bs
            html <- Pandoc.writeHtml5 def pandoc
            pure $ GetDocsResponse html

instance ToSample GetDocsResponse where
    toSamples Proxy = singleSample $ GetDocsResponse "Documentation (In HTML)."

instance ToMarkup GetDocsResponse where
    toMarkup (GetDocsResponse html) = toMarkup html

instance ToSample TimeZone where
    toSamples Proxy = singleSample utc

instance ToSample BaseUrl where
    toSamples Proxy = singleSample $ BaseUrl Https "intray.cs-syd.eu" 8000 ""

instance ToSample EmailAddress where
    toSamples Proxy = singleSample $ unsafeEmailAddress "user" "example.com"

instance ToSample TriggerType

instance ToSample JSON.Value where
    toSamples Proxy = singleSample $ object ["Here" .=( "Be"::Text), "A" .= ("Value"::Text)]
