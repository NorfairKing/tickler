{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.Commands.Review
    ( review
    ) where

import Import

import Tickler.Cli.Commands.Done
import Tickler.Cli.Commands.Show
import Tickler.Cli.OptParse
import Tickler.Cli.Prompt
import Tickler.Cli.Store
import Tickler.Cli.Sync

review :: CliM ()
review = do
    showItem
    res <- liftIO $ prompt "done [y/N]"
    let showSize = do
            s <- syncAndReturn storeSize
            liftIO $ putStrLn $ unwords [show s, "items remaining"]
    let cont = do
            doneItem
            showSize
            review
        stop = pure ()
    case res of
        "y" -> cont
        "Y" -> cont
        "n" -> stop
        "N" -> stop
        _ -> do
            showSize
            review
