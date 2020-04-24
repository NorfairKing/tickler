{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Gen
  ( module Tickler.API.Gen
  , module Tickler.API.Account.Gen
  , module Tickler.API.Admin.Gen
  , module Tickler.API.Protected.Gen
  ) where

import Import
import Tickler.API
import Tickler.API.Account.Gen ()
import Tickler.API.Admin.Gen ()
import Tickler.API.Protected.Gen ()
import Tickler.Data.Gen ()

instance GenValid Registration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid LoginForm where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ChangePassphrase where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Pricing where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
