{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.DeleteTriggereds
  ( serveDeleteTriggereds
  ) where

import Import

import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveDeleteTriggereds :: AuthCookie -> TicklerHandler NoContent
serveDeleteTriggereds AuthCookie {..} = do
  runDb $ deleteWhere [TriggeredItemUserId ==. authCookieUserUUID]
  pure NoContent
