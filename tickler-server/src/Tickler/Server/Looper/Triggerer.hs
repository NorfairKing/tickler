{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Triggerer
    ( runTriggerer
    ) where

import Import

import Control.Concurrent

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool
import Data.Time
import Database.Persist.Sqlite

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runTriggerer :: TriggerSettings -> Looper ()
runTriggerer TriggerSettings = do
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
                , triggeredItemScheduled = ticklerItemScheduled
                }
        -- TODO if something goes wrong here, we should rollback the transaction
        unless (null items) $
            deleteWhere [TicklerItemId <-. map entityKey items]
