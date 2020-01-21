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

import Tickler.API
import Tickler.Data.Gen ()

import Tickler.API.Account.Gen ()
import Tickler.API.Admin.Gen ()
import Tickler.API.Protected.Gen ()

instance GenValid Registration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
