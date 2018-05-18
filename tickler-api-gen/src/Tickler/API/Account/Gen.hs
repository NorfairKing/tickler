{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Tickler.API.Account.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Tickler.API
import Tickler.Data.Gen ()

import Tickler.API.Admin.Gen ()

instance GenUnchecked AccountInfo

instance GenValid AccountInfo where
    genValid =
        (AccountInfo <$> genValid <*> genValid <*> genValid <*> genValid <*>
         genValid) `suchThat`
        isValid
