{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Data.EmailAddress
  ( EmailAddress
  , normalizeEmail
  , unsafeEmailAddress
  , emailValidateFromText
  , emailValidateFromString
  , emailAddressFromText
  , emailAddressFromString
  , emailAddressText
  , emailAddressByteString
  , domainPart
  , localPart
  ) where

import Import

import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE (lenientDecode)
import qualified Text.Email.Validate as Email

import Database.Persist
import Database.Persist.Sql

newtype EmailAddress =
  EmailAddress
    { unEmailAddress :: Email.EmailAddress
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity EmailAddress where
  validate eaa =
    mconcat
      [ annotate (emailAddressText eaa) "emailAddressText"
      , check
          (normalizeEmail (emailAddressText eaa) == emailAddressText eaa)
          "The contained email address is normalised."
      , check
          (emailValidateFromText (emailAddressText eaa) == Right eaa)
          "The contained email address validates to the same email address."
      ]

instance PersistField EmailAddress where
  toPersistValue :: EmailAddress -> PersistValue
  toPersistValue = toPersistValue . emailAddressText
  fromPersistValue :: PersistValue -> Either Text EmailAddress
  fromPersistValue = left T.pack . emailValidateFromText <=< fromPersistValue

instance PersistFieldSql EmailAddress where
  sqlType :: Proxy EmailAddress -> SqlType
  sqlType Proxy = sqlType (Proxy :: Proxy Text)

instance FromJSON EmailAddress where
  parseJSON =
    withText "EmailAddress" $ \t ->
      case emailValidateFromText t of
        Left err -> fail $ show err
        Right ea -> pure ea

instance ToJSON EmailAddress where
  toJSON = JSON.String . emailAddressText

normalizeEmail :: Text -> Text
normalizeEmail = T.map Char.toLower

-- | Wrapper around 'EmailValidate.validate'.
--
-- >>> validate "foo@gmail.com"
-- Right "foo@gmail.com"
-- >>> import Data.Either (isLeft)
-- >>> isLeft $ validate "not an email address"
-- True
emailValidate :: ByteString -> Either String EmailAddress
emailValidate = fmap EmailAddress . Email.validate

-- | Wrapper around 'EmailValidate.emailAddress'.
--
-- Similar to 'validate', but returns 'Nothing' if the email address fails to
-- parse.
--
-- >>> emailAddress "foo@gmail.com"
-- Just "foo@gmail.com"
-- >>> emailAddress "not an email address"
-- Nothing
emailAddress :: ByteString -> Maybe EmailAddress
emailAddress = fmap EmailAddress . Email.emailAddress

-- | Create an 'EmailAddress' from a 'Text' value.  See 'validate'.
emailValidateFromText :: Text -> Either String EmailAddress
emailValidateFromText = emailValidate . TE.encodeUtf8

-- | Create an 'EmailAddress' from a 'Text' value.  See 'emailAddress'.
emailAddressFromText :: Text -> Maybe EmailAddress
emailAddressFromText = emailAddress . TE.encodeUtf8

-- | Create an 'EmailAddress' from a 'String' value.  See 'validate'.
emailValidateFromString :: String -> Either String EmailAddress
emailValidateFromString = emailValidateFromText . T.pack

-- | Create an 'EmailAddress' from a 'String' value.  See 'emailAddress'.
emailAddressFromString :: String -> Maybe EmailAddress
emailAddressFromString = emailAddressFromText . T.pack

-- | Wrapper around 'EmailValidate.unsafeEmailAddress'.
--
-- Unsafely create an 'EmailAddress' from a local part and a domain part.  The
-- first argument is the local part, and the second argument is the domain
-- part.
--
-- For example, in the email address @foo\@gmail.com@, the local part is @foo@
-- and the domain part is @gmail.com@.
--
-- >>> unsafeEmailAddress "foo" "gmail.com"
-- "foo@gmail.com"
unsafeEmailAddress ::
     ByteString -- ^ Local part
  -> ByteString -- ^ Domain part
  -> EmailAddress
unsafeEmailAddress = (EmailAddress .) . Email.unsafeEmailAddress

-- | Wrapper around 'EmailValidate.localPart'.
--
-- Extracts the local part from an email address.
--
-- For example, in the email address @foo\@gmail.com@, the local part is @foo@.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> localPart email
-- "foo"
localPart :: EmailAddress -> ByteString
localPart = Email.localPart . unEmailAddress

-- | Wrapper around 'EmailValidate.domainPart'.
--
-- Extracts the domain part from an email address.
--
-- For example, in the email address @foo\@gmail.com@, the domain part is
-- @gmail.com@.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> domainPart email
-- "gmail.com"
domainPart :: EmailAddress -> ByteString
domainPart = Email.domainPart . unEmailAddress

-- | Wrapper around 'EmailValidate.toByteString'.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> toByteString email
-- "foo@gmail.com"
emailAddressByteString :: EmailAddress -> ByteString
emailAddressByteString = Email.toByteString . unEmailAddress

-- | Convert an email address to 'Text'.
--
-- This assumes the 'EmailAddress' is UTF8-encoded.
--
-- >>> let email = unsafeEmailAddress "foo" "gmail.com"
-- >>> email
-- "foo@gmail.com"
-- >>> toText email
-- "foo@gmail.com"
emailAddressText :: EmailAddress -> Text
emailAddressText = TE.decodeUtf8With TE.lenientDecode . emailAddressByteString
