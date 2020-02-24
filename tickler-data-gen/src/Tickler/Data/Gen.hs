{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Mergeful ()
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

instance GenValid Username where
  genValid = do
    username <- parseUsername <$> textGen
    case username of
      Just name -> pure name
      Nothing -> genValid
    where
      textGen =
        T.pack <$>
        ((:) <$> charGen <*> ((:) <$> charGen <*> ((:) <$> charGen <*> genListOf charGen)))
      charGen = choose ('\NUL', '\255') `suchThat` validUsernameChar
  shrinkValid = shrinkValidStructurally

instance GenValid HashedPassword where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid User where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked UserSettings

instance GenValid UserSettings

instance GenUnchecked ItemType

instance GenValid ItemType

instance GenValid TicklerItem where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TriggeredItem where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked TriggerType

instance GenValid TriggerType

instance GenUnchecked UserTrigger

instance GenValid UserTrigger where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenUnchecked Scheme

instance GenValid Scheme

instance GenValid BaseUrl where
  genValid = do
    baseUrlScheme <- genValid
    baseUrlHost <- (:) <$> validChar <*> validString
    let baseUrlPort =
          case baseUrlScheme of
            Http -> 80
            Https -> 443
    baseUrlPath <- validString
    pure BaseUrl {..}
    where
      validString = genListOf validChar
      validChar = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.~"
  shrinkValid = shrinkValidStructurally

instance GenValid IntrayTrigger where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

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
      genChar = elements $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.~"
  shrinkValid _ = []

instance GenValid EmailVerificationKey where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EmailTrigger where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid VerificationEmail where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EmailStatus where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Email where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Recurrence where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
