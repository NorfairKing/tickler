{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Protected.Gen where

import Import
import Intray.Data.Gen ()
import Tickler.API.Protected.Types
import Tickler.Data.Gen ()

instance GenValid Tickle

instance GenValid ItemInfo

instance GenValid IntrayTriggerInfo

instance GenValid EmailTriggerInfo

instance GenValid AddIntrayTrigger

instance GenValid AddEmailTrigger
