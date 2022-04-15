{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Account.Gen where

import Import
import Tickler.API
import Tickler.API.Admin.Gen ()
import Tickler.Data.Gen ()

instance GenValid AccountInfo

instance GenValid PaidStatus

instance GenValid AccountSettings
