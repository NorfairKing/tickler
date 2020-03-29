{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredEmailScheduler
  ( runTriggeredEmailScheduler
  ) where

import Import

import Control.Monad.Logger
import Database.Persist

import Tickler.Data

import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredEmailScheduler :: () -> Looper ()
runTriggeredEmailScheduler _ = do
  logInfoNS "TriggeredEmailScheduler" "Starting scheduling TriggeredEmails from triggered items."
  tis <- runDb $ selectList [] [Asc TriggeredItemScheduledDay, Asc TriggeredItemScheduledTime]
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
          met <- runDb $ selectFirst [EmailTriggerIdentifier ==. userTriggerTriggerId ut] []
          case met of
            Nothing -> pure Nothing
            Just (Entity _ EmailTrigger {..}) ->
              if emailTriggerVerified
                then do
                  mte <-
                    runDb $
                    getBy $
                    UniqueTriggeredEmail (triggeredItemIdentifier ti) (userTriggerTriggerId ut)
                  pure $
                    case mte of
                      Nothing ->
                        Just
                          TriggeredEmail
                            { triggeredEmailItem = triggeredItemIdentifier ti
                            , triggeredEmailTrigger = userTriggerTriggerId ut
                            , triggeredEmailEmail = Nothing
                            , triggeredEmailError = Nothing
                            }
                      Just _ -> Nothing
                else pure Nothing
  runDb $ insertMany_ tes
  logInfoNS "TriggeredEmailScheduler" "Finished scheduling TriggeredEmails from triggered items."
