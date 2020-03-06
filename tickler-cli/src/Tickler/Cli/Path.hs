module Tickler.Cli.Path
  ( sessionPath
  , storePath
  ) where

import Import

import Tickler.Cli.OptParse

sessionPath :: CliM (Path Abs File)
sessionPath = do
  d <- asks setCacheDir
  resolveFile d "session.cookie"

storePath :: CliM (Path Abs File)
storePath = do
  d <- asks setDataDir
  resolveFile d "store.json"
