module Tickler.Cli.Store
  ( Store(..)
  , readStore
  , readStoreOrEmpty
  , writeStore
  ) where

import Import

import Tickler.Client

import Tickler.Cli.JSON
import Tickler.Cli.OptParse
import Tickler.Cli.Path

{-# ANN module "HLint: ignore Use &&" #-}

readStore :: CliM (Maybe Store)
readStore = storePath >>= readJSON

readStoreOrEmpty :: CliM Store
readStoreOrEmpty = fromMaybe emptyStore <$> readStore

writeStore :: Store -> CliM ()
writeStore s = storePath >>= (`writeJSON` s)
