{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.EmailVerificationKey where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Base16 as SB16
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist
import Database.Persist.Sql
import Import
import System.Random

newtype EmailVerificationKey
  = EmailVerificationKey ByteString
  deriving (Eq, Generic, PersistField, PersistFieldSql)

instance Validity EmailVerificationKey

instance Show EmailVerificationKey where
  show = T.unpack . emailVerificationKeyText

instance Read EmailVerificationKey where
  readsPrec _ s =
    case parseEmailVerificationKeyText $ T.pack s of
      Nothing -> []
      Just evk -> [(evk, "")]

emailVerificationKeyText :: EmailVerificationKey -> Text
emailVerificationKeyText (EmailVerificationKey bs) = TE.decodeUtf8 $ SB16.encode bs

parseEmailVerificationKeyText :: Text -> Maybe EmailVerificationKey
parseEmailVerificationKeyText t =
  case SB16.decode $ TE.encodeUtf8 t of
    Right d -> Just $ EmailVerificationKey d
    Left _ -> Nothing

generateRandomVerificationKey :: IO EmailVerificationKey
generateRandomVerificationKey = EmailVerificationKey . SB.pack <$> replicateM 16 randomIO
