{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Store
    ( Store(..)
    , readStore
    , readStoreOrEmpty
    , writeStore
    ) where

import Import

import Data.Mergeless

import Tickler.API

import Tickler.Cli.JSON
import Tickler.Cli.OptParse
import Tickler.Cli.Path

{-# ANN module "HLint: ignore Use &&" #-}

readStore :: CliM (Maybe (Store ItemUUID TypedItem))
readStore = storePath >>= readJSON

readStoreOrEmpty :: CliM (Store ItemUUID TypedItem)
readStoreOrEmpty = fromMaybe emptyStore <$> readStore

writeStore :: (Store ItemUUID TypedItem) -> CliM ()
writeStore s = do
    storePath >>= (`writeJSON` s)
