{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Gen where

import Import

import Data.Char as Char
import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Text as T

import Database.Persist.Sql
import Servant.Client.Core

import Intray.API.Gen ()

import Tickler.Data

instance ToBackendKey SqlBackend a => GenUnchecked (Key a) where
    genUnchecked = toSqlKey <$> genUnchecked
    shrinkUnchecked = map toSqlKey . shrinkUnchecked . fromSqlKey

instance ToBackendKey SqlBackend a => GenValid (Key a)

instance GenUnchecked Username

instance GenValid Username where
    genValid = do
        username <- parseUsername <$> textGen
        case username of
            Just name -> pure name
            Nothing -> genValid
      where
        textGen =
            T.pack <$>
            ((:) <$> charGen <*>
             ((:) <$> charGen <*> ((:) <$> charGen <*> genListOf charGen)))
        charGen = genValid `suchThat` validUsernameChar

instance GenUnchecked HashedPassword

instance GenValid HashedPassword

instance GenUnchecked User

instance GenValid User where
    genValid = genValidStructurally

instance GenUnchecked UserSettings

instance GenValid UserSettings

instance GenUnchecked ItemType

instance GenValid ItemType

instance GenUnchecked TicklerItem

instance GenValid TicklerItem where
    genValid = genValidStructurally

instance GenUnchecked TriggeredItem

instance GenValid TriggeredItem where
    genValid = genValidStructurally

instance GenUnchecked TriggerType

instance GenValid TriggerType

instance GenUnchecked UserTrigger

instance GenValid UserTrigger where
    genValid = genValidStructurally

instance GenUnchecked Scheme

instance GenValid Scheme

instance GenUnchecked BaseUrl

instance GenValid BaseUrl where
    genValid =
        BaseUrl <$> genValid <*> ((:) <$> validChar <*> validString) <*>
        (((`mod` 65536) . abs) <$> genValid) <*>
        validString `suchThat` isValid
      where
        validString = genListOf validChar
        validChar =
            genValid `suchThat`
            (\c -> Char.isAlphaNum c && Char.isLatin1 c && c /= ' ')

instance GenUnchecked IntrayTrigger

instance GenValid IntrayTrigger where
    genValid = genValidStructurally

instance GenUnchecked EmailAddress where
    genUnchecked = unsafeEmailAddress <$> genUnchecked <*> genUnchecked
    shrinkUnchecked _ = []

instance GenValid EmailAddress where
    genValid = do
        domain <- genS
        host <- genS
        let eac = normalizeEmail $ T.pack $ concat [domain, "@", host]
        case emailAddressFromText eac >>= constructValid of
            Nothing -> scale (+ 5) genValid
            Just ea -> pure ea
      where
        genS = (:) <$> genChar <*> genListOf genChar
        genChar =
            genValid `suchThat` (\c -> Char.isAlphaNum c && Char.isLatin1 c)

instance GenUnchecked EmailVerificationKey

instance GenValid EmailVerificationKey where
    genValid = genValidStructurally

instance GenUnchecked EmailTrigger

instance GenValid EmailTrigger where
    genValid = genValidStructurally

instance GenUnchecked VerificationEmail

instance GenValid VerificationEmail where
    genValid = genValidStructurally

instance GenUnchecked EmailStatus

instance GenValid EmailStatus

instance GenUnchecked Email

instance GenValid Email where
    genValid = genValidStructurally

instance GenUnchecked Recurrence

instance GenValid Recurrence
