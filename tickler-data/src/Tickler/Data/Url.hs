{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Url
    ( BaseUrl(..)
    ) where

import Import

import Data.Aeson as JSON

import qualified Data.ByteString.Lazy as LB
import Data.Char as Char
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.UUID.Typed

import Servant.Client.Core

import Database.Persist
import Database.Persist.Sql

instance Validity Scheme

instance Validity BaseUrl where
    validate BaseUrl {..} =
        mconcat
            [ annotate baseUrlScheme "baseUrlScheme"
            , annotate baseUrlHost "baseUrlHost"
            , annotate baseUrlPort "baseUrlPort"
            , annotate baseUrlPath "baseUrlPath"
            , declare "The hostname is not empty" $ not $ null baseUrlHost
            , declare "The port is positive" $ baseUrlPort >= 0
            , declare "The port is less than 65536" $ baseUrlPort <= 65536
            , declare "The hostname does not contain spaces" $
              not $ any Char.isSpace baseUrlHost
            , declare "The host is entirely within Latin1" $
              all Char.isLatin1 baseUrlHost
            , declare "The host is entirely alphanumeric" $
              all Char.isAlphaNum baseUrlHost
            , declare "The path does not contain spaces" $
              not $ any Char.isSpace baseUrlPath
            , declare "The path is entirely within Latin1" $
              all Char.isLatin1 baseUrlPath
            , declare "The path is entirely alphanumeric" $
              all Char.isAlphaNum baseUrlPath
            ]

instance PersistField BaseUrl where
    toPersistValue = PersistText . T.pack . showBaseUrl
    fromPersistValue (PersistText t) =
        left (T.pack . show) $ parseBaseUrl $ T.unpack t
    fromPersistValue _ = Left "Invalid persist field type to parse BaseUrl"

instance PersistFieldSql BaseUrl where
    sqlType Proxy = SqlString

instance FromJSON BaseUrl where
    parseJSON =
        withText "BaseUrl" $ \t ->
            case parseBaseUrl $ T.unpack t of
                Left err -> fail $ show err
                Right burl -> pure burl

instance ToJSON BaseUrl where
    toJSON = JSON.String . T.pack . showBaseUrl
