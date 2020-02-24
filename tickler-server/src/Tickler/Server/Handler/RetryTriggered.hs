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

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveRetryTriggered :: AuthCookie -> [ItemUUID] -> TicklerHandler NoContent
serveRetryTriggered AuthCookie {..} ids = do
  runDb $ updateWhere [TriggeredIntrayItemItem <-. ids] [TriggeredIntrayItemError =. Nothing]
  runDb $ updateWhere [TriggeredEmailItem <-. ids] [TriggeredEmailError =. Nothing]
  pure NoContent
