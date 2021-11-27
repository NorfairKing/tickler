{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PostRetryTriggered
  ( servePostRetryTriggered,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostRetryTriggered :: AuthCookie -> [ItemUUID] -> TicklerHandler NoContent
servePostRetryTriggered AuthCookie {..} ids = do
  runDb $ do
    forM_ ids $ \i ->
      updateWhere [TriggeredIntrayItemItem ==. i] [TriggeredIntrayItemError =. Nothing]
    forM_ ids $ \i -> updateWhere [TriggeredEmailItem ==. i] [TriggeredEmailError =. Nothing]
  pure NoContent
