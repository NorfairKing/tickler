{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Tickler.API.Protected.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Tickler.Data.Gen ()

import Tickler.API.Protected.Types

instance GenUnchecked TypedItem

instance GenValid TypedItem where
    genValid = (TypedItem <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked a => GenUnchecked (ItemInfo a)

instance GenValid a => GenValid (ItemInfo a) where
    genValid =
        (ItemInfo <$> genValid <*> genValid <*> genValid <*> genValid) `suchThat`
        isValid

instance GenUnchecked AddItem

instance GenValid AddItem where
    genValid = (AddItem <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked SyncRequest

instance GenValid SyncRequest where
    genValid =
        (SyncRequest <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked NewSyncItem

instance GenValid NewSyncItem where
    genValid =
        (NewSyncItem <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked SyncResponse

instance GenValid SyncResponse where
    genValid =
        (SyncResponse <$> genValid <*> genValid <*> genValid) `suchThat` isValid
