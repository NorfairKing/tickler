{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.PostRetryTriggered
  ( servePostRetryTriggered
  ) where

import Import

import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostRetryTriggered :: AuthCookie -> [ItemUUID] -> TicklerHandler NoContent
servePostRetryTriggered AuthCookie {..} ids = do
  runDb $ do
    forM_ ids $ \i ->
      updateWhere [TriggeredIntrayItemItem ==. i] [TriggeredIntrayItemError =. Nothing]
    forM_ ids $ \i -> updateWhere [TriggeredEmailItem ==. i] [TriggeredEmailError =. Nothing]
  pure NoContent
