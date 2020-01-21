module Tickler.Cli.Path
  ( sessionPath
  , lastSeenItemPath
  , storePath
  ) where

import Import

import Tickler.Cli.OptParse

ticklerDir :: CliM (Path Abs Dir)
ticklerDir = asks setTicklerDir

sessionPath :: CliM (Path Abs File)
sessionPath = do
  d <- ticklerDir
  resolveFile d "session.cookie"

lastSeenItemPath :: CliM (Path Abs File)
lastSeenItemPath = do
  d <- ticklerDir
  resolveFile d "last-seen-item.json"

storePath :: CliM (Path Abs File)
storePath = do
  d <- ticklerDir
  resolveFile d "store.json"
