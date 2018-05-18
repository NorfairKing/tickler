{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Admin.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Tickler.API
import Tickler.Data.Gen ()

instance GenUnchecked AdminStats

instance GenValid AdminStats where
    genValid = (AdminStats <$> genValid <*> genValid) `suchThat` isValid
