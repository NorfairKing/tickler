{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

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

serveGetTriggers :: AuthCookie -> TicklerHandler [TriggerInfo TypedTriggerInfo]
serveGetTriggers AuthCookie {..} = do
  uts <- runDb $ selectList [UserTriggerUserId ==. authCookieUserUUID] []
  runDb
    $ fmap catMaybes
    $ forM uts
    $ \(Entity _ UserTrigger {..}) ->
      liftA2
        mplus
        ( fmap (makeIntrayTriggerInfo . entityVal)
            <$> selectFirst [IntrayTriggerIdentifier ==. userTriggerTriggerId] []
        )
        ( fmap (makeEmailTriggerInfo . entityVal)
            <$> selectFirst [EmailTriggerIdentifier ==. userTriggerTriggerId] []
        )
