{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.GetTriggers
  ( serveGetTriggers,
  )
where

import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Trigger
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveGetTriggers :: AuthCookie -> TicklerHandler [TriggerInfo]
serveGetTriggers AuthCookie {..} =
  runDB $
    liftA2
      mplus
      (fmap (makeIntrayTriggerInfo . entityVal) <$> selectList [IntrayTriggerUser ==. authCookieUserUUID] [])
      (fmap (makeEmailTriggerInfo . entityVal) <$> selectList [EmailTriggerUser ==. authCookieUserUUID] [])
