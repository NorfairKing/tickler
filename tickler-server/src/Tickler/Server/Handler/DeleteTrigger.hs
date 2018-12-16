{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.DeleteTrigger
    ( serveDeleteTrigger
    ) where

import Import

import Database.Persist

import Servant
import Servant.Auth.Server as Auth

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveDeleteTrigger ::
       AuthResult AuthCookie -> TriggerUUID -> TicklerHandler NoContent
serveDeleteTrigger (Authenticated AuthCookie {..}) uuid = do
    ment1 <- runDb $ getBy $ UniqueIntrayTrigger uuid
    case ment1 of
        Nothing -> do
            ment2 <- runDb $ getBy $ UniqueEmailTrigger uuid
            case ment2 of
                Nothing -> throwAll err404 {errBody = "Trigger not found."}
                Just (Entity i _) -> runDb $ delete i
        Just (Entity i _) -> runDb $ delete i
    pure NoContent
serveDeleteTrigger _ _ = throwAll err401
