{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.GetTriggers
  ( serveGetTriggers
  ) where

import Import

import Database.Persist

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Trigger
import Tickler.Server.Handler.Utils

serveGetTriggers :: AuthCookie -> TicklerHandler [TriggerInfo TypedTriggerInfo]
serveGetTriggers AuthCookie {..} = do
  uts <- runDb $ selectList [UserTriggerUserId ==. authCookieUserUUID] []
  runDb $
    fmap catMaybes $
    forM uts $ \(Entity _ UserTrigger {..}) ->
      liftA2
        mplus
        (fmap (makeIntrayTriggerInfo . entityVal) <$>
         selectFirst [IntrayTriggerIdentifier ==. userTriggerTriggerId] [])
        (fmap (makeEmailTriggerInfo . entityVal) <$>
         selectFirst [EmailTriggerIdentifier ==. userTriggerTriggerId] [])
