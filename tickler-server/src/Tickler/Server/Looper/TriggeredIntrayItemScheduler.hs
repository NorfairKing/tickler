{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredIntrayItemScheduler
    ( runTriggeredIntrayItemScheduler
    ) where

import Import

import Control.Concurrent

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool
import Database.Persist.Sqlite

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runTriggeredIntrayItemScheduler :: () -> Looper ()
runTriggeredIntrayItemScheduler _ = do
    logInfoNS
        "TriggeredIntrayScheduler"
        "Starting scheduling TriggeredIntrayItems from triggered items."
    tis <- runDb $ selectList [] [Asc TriggeredItemScheduled]
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
                                          Nothing
                                    }
                            Just _ -> Nothing
    runDb $ insertMany_ tes
    logInfoNS
        "TriggeredIntrayScheduler"
        "Finished scheduling TriggeredIntrayItems from triggered items."
