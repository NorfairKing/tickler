{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tickler.Server.Handler.Protected.GetTrigger where

import Database.Persist
import Import
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Tickler.API
import Tickler.Server.Handler.Trigger
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveGetTrigger :: AuthCookie -> TriggerUUID -> TicklerHandler TriggerInfo
serveGetTrigger _ uuid = do
  mit <- fmap (makeIntrayTriggerInfo . entityVal) <$> runDb (getBy $ UniqueIntrayTrigger uuid)
  case mit of
    Nothing -> do
      mit' <- fmap (makeEmailTriggerInfo . entityVal) <$> runDb (getBy $ UniqueEmailTrigger uuid)
      case mit' of
        Nothing -> throwAll err404
        Just ti' -> pure ti'
    Just ti' -> pure ti'
