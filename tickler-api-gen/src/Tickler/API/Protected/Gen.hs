{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Tickler.API.Protected.Gen where

import Import

import Intray.Data.Gen ()
import Tickler.Data.Gen ()

import Tickler.API.Protected.Types

instance GenUnchecked ItemFilter

instance GenValid ItemFilter

instance GenUnchecked TypedItem

instance GenValid TypedItem where
    genValid = genValidStructurally

instance GenUnchecked a => GenUnchecked (Tickle a)

instance GenValid a => GenValid (Tickle a) where
    genValid = genValidStructurally

instance GenUnchecked TriggerAttempt

instance GenValid TriggerAttempt where
    genValid = genValidStructurally

instance GenUnchecked EmailTriggerResult

instance GenValid EmailTriggerResult where
    genValid = genValidStructurally

instance GenUnchecked IntrayTriggerResult

instance GenValid IntrayTriggerResult where
    genValid = genValidStructurally

instance GenUnchecked TriggeredInfo

instance GenValid TriggeredInfo where
    genValid = genValidStructurally

instance GenUnchecked a => GenUnchecked (ItemInfo a)

instance GenValid a => GenValid (ItemInfo a) where
    genValid = genValidStructurally

instance GenUnchecked a => GenUnchecked (TriggerInfo a)

instance GenValid a => GenValid (TriggerInfo a) where
    genValid = genValidStructurally

instance GenUnchecked TypedTriggerInfo

instance GenValid TypedTriggerInfo where
    genValid = genValidStructurally

instance GenUnchecked IntrayTriggerInfo

instance GenValid IntrayTriggerInfo where
    genValid = genValidStructurally

instance GenUnchecked EmailTriggerInfo

instance GenValid EmailTriggerInfo where
    genValid = genValidStructurally

instance GenUnchecked AddIntrayTrigger

instance GenValid AddIntrayTrigger where
    genValid = genValidStructurally

instance GenUnchecked AddEmailTrigger

instance GenValid AddEmailTrigger where
    genValid = genValidStructurally

instance GenUnchecked SyncRequest

instance GenValid SyncRequest where
    genValid = genValidStructurally

instance GenUnchecked SyncResponse

instance GenValid SyncResponse where
    genValid = genValidStructurally
