{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.Username
  ( Username (),
    parseUsername,
    parseUsernameWithError,
    usernameText,
    validUsernameChar,
  )
where

import Autodocodec
import Data.Aeson as JSON
import qualified Data.Char as Char
import Data.Hashable
import qualified Data.Text as T
import Database.Persist.Sql
import Import

newtype Username = Username
  { usernameText :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec Username)

instance Validity Username where
  validate (Username t) =
    mconcat
      [ check (not (T.null t)) "The username is not empty.",
        check (T.length t >= 3) "The username is at least three characters long.",
        mconcat $
          flip map (zip [1 ..] $ map UsernameChar $ T.unpack t) $
            \(ix, uc@(UsernameChar c)) ->
              annotate uc $ unwords ["character number", show (ix :: Int), "of the username:", show c]
      ]

instance Hashable Username

instance PersistField Username where
  toPersistValue (Username t) = PersistText t
  fromPersistValue (PersistText t) =
    case parseUsername t of
      Nothing -> Left "Text isn't a valid username"
      Just un -> Right un
  fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance FromJSONKey Username where
  fromJSONKey = FromJSONKeyTextParser parseUsername

instance HasCodec Username where
  codec = bimapCodec parseUsernameWithError usernameText codec

parseUsername :: MonadFail m => Text -> m Username
parseUsername t =
  case parseUsernameWithError t of
    Left err -> fail err
    Right un -> pure un

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError t =
  case prettyValidate $ Username t of
    Right un -> Right un
    Left err -> Left err

newtype UsernameChar
  = UsernameChar Char

instance Validity UsernameChar where
  validate (UsernameChar '-') = mempty
  validate (UsernameChar '_') = mempty
  validate (UsernameChar c) =
    mconcat
      [ check (not (Char.isControl c)) "The character is not a control character.",
        check (Char.isAlphaNum c) "The character is alphanumeric.",
        check (Char.isLatin1 c) "The character is part of Latin1."
      ]

validUsernameChar :: Char -> Bool
validUsernameChar = isValid . UsernameChar
