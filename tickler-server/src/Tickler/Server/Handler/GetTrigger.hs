{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetTrigger where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.GetTriggers
import Tickler.Server.Handler.Utils

serveGetTrigger ::
     AuthResult AuthCookie -> TriggerUUID -> TicklerHandler (TriggerInfo TypedTriggerInfo)
serveGetTrigger (Authenticated AuthCookie {..}) uuid = do
  mit <- fmap (makeIntrayTriggerInfo . entityVal) <$> runDb (getBy $ UniqueIntrayTrigger uuid)
  case mit of
    Nothing -> do
      mit' <- fmap (makeEmailTriggerInfo . entityVal) <$> runDb (getBy $ UniqueEmailTrigger uuid)
      case mit' of
        Nothing -> throwAll $ err404 {errBody = "Trigger not found."}
        Just ti' -> pure ti'
    Just ti' -> pure ti'
serveGetTrigger _ _ = throwAll err401
