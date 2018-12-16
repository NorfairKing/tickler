{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Tickler.API.Account.Gen where

import Import

import Tickler.API
import Tickler.Data.Gen ()

import Tickler.API.Admin.Gen ()

instance GenUnchecked AccountInfo

instance GenValid AccountInfo where
    genValid = genValidStructurally

instance GenUnchecked AccountSettings

instance GenValid AccountSettings where
    genValid = genValidStructurally
