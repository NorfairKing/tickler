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
import Web.HttpApiData
import Web.PathPieces

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
  toPersistValue = toPersistValue . usernameText
  fromPersistValue pv = do
    t <- fromPersistValue pv
    left T.pack $ parseUsernameWithError t

instance PersistFieldSql Username where
  sqlType _ = SqlString

instance FromJSONKey Username where
  fromJSONKey = FromJSONKeyTextParser parseUsername

instance HasCodec Username where
  codec = bimapCodec parseUsernameWithError usernameText codec

instance PathPiece Username where
  fromPathPiece = parseUsername
  toPathPiece = usernameText

instance ToHttpApiData Username where
  toUrlPiece = usernameText
  toQueryParam = usernameText

instance FromHttpApiData Username where
  parseUrlPiece = left T.pack . parseUsernameWithError
  parseQueryParam = left T.pack . parseUsernameWithError

parseUsername :: MonadFail m => Text -> m Username
parseUsername t =
  case parseUsernameWithError t of
    Left err -> fail err
    Right un -> pure un

parseUsernameWithError :: Text -> Either String Username
parseUsernameWithError = prettyValidate . Username

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
