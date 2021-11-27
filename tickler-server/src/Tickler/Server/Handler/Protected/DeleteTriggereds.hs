{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.DeleteTriggereds
  ( serveDeleteTriggereds,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveDeleteTriggereds :: AuthCookie -> TicklerHandler NoContent
serveDeleteTriggereds AuthCookie {..} = do
  runDb $ deleteWhere [TriggeredItemUserId ==. authCookieUserUUID]
  pure NoContent
