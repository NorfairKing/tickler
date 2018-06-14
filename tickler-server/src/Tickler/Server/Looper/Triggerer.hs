{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Triggerer
    ( runTriggerer
    ) where

import Import

import Control.Monad.Logger
import Data.Time
import Database.Persist.Sqlite

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runTriggerer :: TriggererSettings -> Looper ()
runTriggerer TriggererSettings = do
    logInfoNS "Triggerer" "Starting triggering tickles."
    now <- liftIO getCurrentTime
    runDb $ do
        items <-
            selectList [TicklerItemScheduled <=. now] [Asc TicklerItemScheduled]
        insertMany_ $
            flip map items $ \(Entity _ TicklerItem {..}) ->
                TriggeredItem
                { triggeredItemIdentifier = ticklerItemIdentifier
                , triggeredItemUserId = ticklerItemUserId
                , triggeredItemType = ticklerItemType
                , triggeredItemContents = ticklerItemContents
                , triggeredItemCreated = ticklerItemCreated
                , triggeredItemSynced = ticklerItemSynced
                , triggeredItemScheduled = ticklerItemScheduled
                , triggeredItemRecurrence = ticklerItemRecurrence
                , triggeredItemTriggered = now
                }
        -- TODO if something goes wrong here, we should rollback the transaction
        unless (null items) $
            deleteWhere [TicklerItemId <-. map entityKey items]
    logInfoNS "Triggerer" "Finished triggering tickles."
