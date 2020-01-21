module Tickler.Data.AccountUUID
  ( AccountUUID
  , module Data.UUID.Typed
  ) where

import Data.UUID.Typed

import Tickler.Data.UUID ()

type AccountUUID = UUID User

data User
