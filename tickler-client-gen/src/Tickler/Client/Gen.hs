{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Client.Gen where

import Import

import Tickler.Client.Store

import Tickler.API.Gen ()

instance GenUnchecked StoreItem

instance GenValid StoreItem

instance GenUnchecked Store

instance GenValid Store where
    genValid = Store <$> genValid
