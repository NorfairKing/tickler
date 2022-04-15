{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PostAddItem (servePostAddItem) where

import Data.Time
import Data.UUID.Typed
import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

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
      runDb $ insert_ $ makeTicklerItem authCookieUserUUID uuid now ti
      pure uuid
