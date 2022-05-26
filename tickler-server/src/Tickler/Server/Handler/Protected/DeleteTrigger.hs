{-# LANGUAGE OverloadedStrings #-}

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
serveDeleteTrigger _ uuid = do
  ment1 <- runDB $ getBy $ UniqueIntrayTrigger uuid
  case ment1 of
    Nothing -> do
      ment2 <- runDB $ getBy $ UniqueEmailTrigger uuid
      case ment2 of
        Nothing -> throwAll err404
        Just (Entity i _) -> runDB $ delete i
    Just (Entity i _) -> runDB $ delete i
  pure NoContent
