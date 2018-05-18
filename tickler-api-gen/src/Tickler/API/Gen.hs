{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Tickler.API.Gen
    ( module Tickler.API.Gen
    , module Tickler.API.Account.Gen
    , module Tickler.API.Admin.Gen
    , module Tickler.API.Protected.Gen
    ) where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Tickler.API
import Tickler.Data.Gen ()

import Tickler.API.Account.Gen ()
import Tickler.API.Admin.Gen ()
import Tickler.API.Protected.Gen ()


instance GenUnchecked Registration

instance GenValid Registration where
    genValid = (Registration <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked LoginForm

instance GenValid LoginForm where
    genValid = (LoginForm <$> genValid <*> genValid) `suchThat` isValid
