{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Commands.Add
  ( add
  ) where

import Import

import Data.Mergeful
import Data.Time

import Tickler.Client

import Tickler.Cli.OptParse
import Tickler.Cli.Sync

add :: AddSettings -> CliM ()
add AddSettings {..} =
  withStoreAndSync $ \s -> do
    now <- liftIO getCurrentTime
    let a =
          AddedItem
            { addedItemCreated = now
            , addedItemContents =
                Tickle
                  { tickleContent = textTypedItem addSetTickleContent
                  , tickleScheduledDay = addSetTickleDate
                  , tickleScheduledTime = addSetTickleTime
                  , tickleRecurrence = Nothing
                  }
            }
    pure $ addTickleToStore s a
