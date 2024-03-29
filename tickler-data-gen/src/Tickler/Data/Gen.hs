{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Persist ()
import Data.GenValidity.Text
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()
import Data.GenValidity.UUID.Typed ()
import qualified Data.Text as T
import Import
import Intray.API.Gen ()
import Servant.Client.Core
import Tickler.Data

instance GenValid Username where
  genValid = do
    username <- parseUsername <$> textGen
    case username of
      Just name -> pure name
      Nothing -> genValid
    where
      textGen =
        T.pack
          <$> ((:) <$> charGen <*> ((:) <$> charGen <*> ((:) <$> charGen <*> genListOf charGen)))
      charGen = choose ('\NUL', '\255') `suchThat` validUsernameChar
  shrinkValid = shrinkValidStructurally

instance GenValid HashedPassword

instance GenValid MinuteOfDay where
  genValid = MinuteOfDay <$> choose (0, 60 * 24 - 1)

instance GenValid User

instance GenValid UserSettings

instance GenValid TicklerItem

instance GenValid TriggeredItem

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

instance GenValid IntrayTrigger

instance GenValid EmailAddress where
  genValid = emailAddress <$> (genValid >>= textStartingWith)

instance GenValid EmailVerificationKey

instance GenValid EmailTrigger

instance GenValid VerificationEmail

instance GenValid EmailStatus

instance GenValid Email

instance GenValid Recurrence
