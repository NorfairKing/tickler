{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.PostAddItem
  ( servePostAddItem
  ) where

import Import

import qualified Data.Mergeful.Timed as Mergeful
import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Item
import Tickler.Server.Types

import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils

servePostAddItem :: AuthCookie -> AddItem -> TicklerHandler ItemUUID
servePostAddItem AuthCookie {..} ti = do
  ups <- getUserPaidStatus authCookieUserUUID
  case ups of
    HasNotPaid i ->
      if i >= 1
        then goAhead
        else throwError err402
    HasPaid _ -> goAhead
    NoPaymentNecessary -> goAhead
  where
    goAhead = do
      now <- liftIO getCurrentTime
      uuid <- liftIO nextRandomUUID
      runDb $ insert_ $ makeTicklerItem authCookieUserUUID uuid now Mergeful.initialServerTime ti
      pure uuid
