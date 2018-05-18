module Tickler.Cli.LastSeen
    ( LastItem(..)
    , writeLastSeen
    , readLastSeen
    , clearLastSeen
    ) where

import Import

import Tickler.Client.Store

import Tickler.Cli.JSON
import Tickler.Cli.OptParse
import Tickler.Cli.Path

readLastSeen :: CliM (Maybe LastItem)
readLastSeen = do
    p <- lastSeenItemPath
    readJSON p

writeLastSeen :: LastItem -> CliM ()
writeLastSeen i = do
    p <- lastSeenItemPath
    writeJSON p i

clearLastSeen :: CliM ()
clearLastSeen = do
    p <- lastSeenItemPath
    liftIO $ ignoringAbsence $ removeFile p
