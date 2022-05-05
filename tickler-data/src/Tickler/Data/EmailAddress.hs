{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Data.EmailAddress
  ( EmailAddress,
    emailAddress,
    emailAddressText,
  )
where

import Autodocodec
import Data.Aeson as JSON
import qualified Data.Char as Char
import Data.String
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Import

newtype EmailAddress = EmailAddress
  { emailAddressText :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (PersistFieldSql)
  deriving (FromJSON, ToJSON) via (Autodocodec EmailAddress)

instance Validity EmailAddress where
  validate ea@(EmailAddress t) =
    mconcat
      [ genericValidate ea,
        declare "it's not empty" $ not $ T.null t,
        declare "it's normalised" $ normalizeEmail t == t
      ]

instance IsString EmailAddress where
  fromString = emailAddress . fromString

instance PersistField EmailAddress where
  toPersistValue :: EmailAddress -> PersistValue
  toPersistValue = toPersistValue . emailAddressText
  fromPersistValue :: PersistValue -> Either Text EmailAddress
  fromPersistValue = fmap emailAddress . fromPersistValue

instance HasCodec EmailAddress where
  codec = dimapCodec EmailAddress emailAddressText codec

emailAddress :: Text -> EmailAddress
emailAddress = EmailAddress . normalizeEmail

normalizeEmail :: Text -> Text
normalizeEmail = T.map Char.toLower
