module Tickler.Cli.Commands.Done
    ( doneItem
    ) where

import Import

import Tickler.Cli.LastSeen
import Tickler.Cli.OptParse
import Tickler.Cli.Store
import Tickler.Cli.Sync

doneItem :: ReaderT Settings IO ()
doneItem = do
    mii <- readLastSeen
    case mii of
        Nothing ->
            liftIO $
            die "Are you sure?, it doesn't look like you showed an item yet."
        Just li -> do
            modifyStoreAndSync $ doneLastItem li
            clearLastSeen
