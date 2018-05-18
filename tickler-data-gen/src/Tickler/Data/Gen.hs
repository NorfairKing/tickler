{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()
import qualified Data.Text as T

import Tickler.Data

instance GenUnchecked ItemUUID

instance GenValid ItemUUID

instance GenUnchecked AccountUUID

instance GenValid AccountUUID

instance GenUnchecked ItemType

instance GenValid ItemType

instance GenUnchecked TicklerItem

instance GenValid TicklerItem where
    genValid =
        (TicklerItem <$> genValid <*> genValid <*> genValid <*> genValid <*>
         genValid <*>
         genValid) `suchThat`
        isValid

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

instance GenUnchecked User

instance GenUnchecked HashedPassword
