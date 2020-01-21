module Tickler.Data.ItemUUID
  ( ItemUUID
  , module Data.UUID.Typed
  ) where

import Data.UUID.Typed

import Tickler.Data.UUID ()

type ItemUUID = UUID Item

data Item
