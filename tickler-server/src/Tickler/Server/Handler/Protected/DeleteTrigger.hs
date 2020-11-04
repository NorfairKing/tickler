{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.DeleteTrigger
  ( serveDeleteTrigger,
  )
where

import Database.Persist
import Import
import Servant
import Servant.Auth.Server
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveDeleteTrigger :: AuthCookie -> TriggerUUID -> TicklerHandler NoContent
serveDeleteTrigger AuthCookie {..} uuid = do
  ment1 <- runDb $ getBy $ UniqueIntrayTrigger uuid
  case ment1 of
    Nothing -> do
      ment2 <- runDb $ getBy $ UniqueEmailTrigger uuid
      case ment2 of
        Nothing -> throwAll err404 {errBody = "Trigger not found."}
        Just (Entity i _) -> runDb $ delete i
    Just (Entity i _) -> runDb $ delete i
  pure NoContent
