{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tickler.Cli.Commands.Add
    ( addItem
    ) where

import Import

import Data.Time

import Tickler.API

import Tickler.Cli.OptParse
import Tickler.Cli.Store
import Tickler.Cli.Sync

addItem :: Text -> CliM ()
addItem t = do
    now <- liftIO getCurrentTime
    modifyStoreAndSync $ addItemToStore (textTypedItem t) now
