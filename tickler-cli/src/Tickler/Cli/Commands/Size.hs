module Tickler.Cli.Commands.Size
    ( size
    ) where

import Import

import Tickler.Cli.OptParse
import Tickler.Cli.Store
import Tickler.Cli.Sync

size :: CliM ()
size = do
    s <- syncAndReturn storeSize
    liftIO $ print (s :: Int)
