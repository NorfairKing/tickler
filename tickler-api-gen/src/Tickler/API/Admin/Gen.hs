{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Admin.Gen where

import Import

import Tickler.API
import Tickler.Data.Gen ()

instance GenUnchecked AdminStats

instance GenValid AdminStats where
    genValid = (AdminStats <$> genValid <*> genValid) `suchThat` isValid
