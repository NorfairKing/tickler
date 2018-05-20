module Tickler.Data.TriggerUUID
    ( TriggerUUID
    , module Data.UUID.Typed
    ) where

import Data.UUID.Typed

import Tickler.Data.UUID ()

type TriggerUUID = UUID Trigger

data Trigger
