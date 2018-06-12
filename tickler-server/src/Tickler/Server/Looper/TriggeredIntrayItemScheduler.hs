{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredIntrayItemScheduler
    ( runTriggeredIntrayItemScheduler
    ) where

import Import

import Control.Monad.Logger
import Database.Persist.Sqlite


import Tickler.Data


import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runTriggeredIntrayItemScheduler :: () -> Looper ()
runTriggeredIntrayItemScheduler _ = do
    logInfoNS
        "TriggeredIntrayScheduler"
        "Starting scheduling TriggeredIntrayItems from triggered items."
    tis <- runDb $ selectList [] [Asc TriggeredItemScheduled]
    liftIO $ print tis
    tes <-
        fmap concat $
        forM tis $ \(Entity _ ti) -> do
            uts <-
                runDb $
                selectList
                    [ UserTriggerUserId ==. triggeredItemUserId ti
                    , UserTriggerTriggerType ==. IntrayTriggerType
                    ]
                    []
            fmap catMaybes $
                forM uts $ \(Entity _ ut) -> do
                    mte <-
                        runDb $
                        getBy $
                        UniqueTriggeredIntrayItem
                            (triggeredItemIdentifier ti)
                            (userTriggerTriggerId ut)
                    pure $
                        case mte of
                            Nothing ->
                                Just
                                    TriggeredIntrayItem
                                    { triggeredIntrayItemItem =
                                          triggeredItemIdentifier ti
                                    , triggeredIntrayItemTrigger =
                                          userTriggerTriggerId ut
                                    , triggeredIntrayItemIntrayItemUUID =
                                          Nothing, triggeredIntrayItemError = Nothing
                                    }
                            Just _ -> Nothing
    runDb $ insertMany_ tes
    logInfoNS
        "TriggeredIntrayScheduler"
        "Finished scheduling TriggeredIntrayItems from triggered items."
