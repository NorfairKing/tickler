{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredEmailScheduler
    ( runTriggeredEmailScheduler
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

runTriggeredEmailScheduler :: () -> Looper ()
runTriggeredEmailScheduler _ = do
    logInfoNS
        "TriggeredEmailScheduler"
        "Starting scheduling TriggeredEmails from triggered items."
    tis <- runDb $ selectList [] [Asc TriggeredItemScheduled]
    tes <-
        fmap concat $
        forM tis $ \(Entity _ ti) -> do
            uts <-
                runDb $
                selectList
                    [ UserTriggerUserId ==. triggeredItemUserId ti
                    , UserTriggerTriggerType ==. EmailTriggerType
                    ]
                    []
            fmap catMaybes $
                forM uts $ \(Entity _ ut) -> do
                    mte <-
                        runDb $
                        getBy $
                        UniqueTriggeredEmail
                            (triggeredItemIdentifier ti)
                            (userTriggerTriggerId ut)
                    pure $
                        case mte of
                            Nothing ->
                                Just
                                    TriggeredEmail
                                    { triggeredEmailItem =
                                          triggeredItemIdentifier ti
                                    , triggeredEmailTrigger =
                                          userTriggerTriggerId ut
                                    , triggeredEmailEmail = Nothing
                                    }
                            Just _ -> Nothing
    runDb $ insertMany_ tes
    logInfoNS
        "TriggeredEmailScheduler"
        "Finished scheduling TriggeredEmails from triggered items."
