{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Commands.Add
  ( add,
  )
where

import Data.Time
import Import
import Tickler.Cli.OptParse
import Tickler.Cli.Sync
import Tickler.Client

add :: AddSettings -> CliM ()
add AddSettings {..} =
  withStoreAndSync $ \s -> do
    now <- liftIO getCurrentTime
    let a =
          AddedItem
            { addedItemCreated = now,
              addedItemContents =
                Tickle
                  { tickleContent = textTypedItem addSetTickleContent,
                    tickleScheduledDay = addSetTickleDate,
                    tickleScheduledTime = addSetTickleTime,
                    tickleRecurrence = Nothing
                  }
            }
    pure $ addTickleToStore s a
