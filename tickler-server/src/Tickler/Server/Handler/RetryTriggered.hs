{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.RetryTriggered
    ( serveRetryTriggered
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveRetryTriggered ::
       AuthResult AuthCookie -> [ItemUUID] -> TicklerHandler NoContent
serveRetryTriggered (Authenticated AuthCookie {..}) ids = do
    runDb $
        updateWhere
            [TriggeredIntrayItemItem <-. ids]
            [TriggeredIntrayItemError =. Nothing]
    runDb $
        updateWhere
            [TriggeredEmailItem <-. ids]
            [TriggeredEmailError =. Nothing]
    pure NoContent
serveRetryTriggered _ _ = throwAll err401
