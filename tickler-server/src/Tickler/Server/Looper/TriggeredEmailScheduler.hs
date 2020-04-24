{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredEmailScheduler
  ( runTriggeredEmailScheduler
  ) where

import Database.Persist
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredEmailScheduler :: () -> Looper ()
runTriggeredEmailScheduler () = do
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
