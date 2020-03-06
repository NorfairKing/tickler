{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.DeleteItem
  ( serveDeleteItem
  ) where

import Import

import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveDeleteItem :: AuthCookie -> ItemUUID -> TicklerHandler NoContent
serveDeleteItem AuthCookie {..} id_ = do
  runDb . deleteBy $ UniqueItemIdentifier id_
  runDb . deleteBy $ UniqueTriggeredItemIdentifier id_
  pure NoContent
