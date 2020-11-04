{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Account.Gen where

import Import
import Tickler.API
import Tickler.API.Admin.Gen ()
import Tickler.Data.Gen ()

instance GenValid AccountInfo where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PaidStatus where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AccountSettings where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
