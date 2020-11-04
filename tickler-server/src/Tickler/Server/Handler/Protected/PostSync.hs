{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PostSync
  ( servePostSync,
  )
where

import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Persistent as Mergeful
import Data.UUID.Typed
import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

servePostSync :: AuthCookie -> SyncRequest -> TicklerHandler SyncResponse
servePostSync AuthCookie {..} req = do
  ups <- getUserPaidStatus authCookieUserUUID
  syncResponseTickles <-
    runDb $
      Mergeful.serverProcessSyncWithCustomIdQuery
        TicklerItemIdentifier
        nextRandomUUID
        TicklerItemServerTime
        [TicklerItemUserId ==. authCookieUserUUID]
        makeTicklerAdded
        ( \_ uuid AddedItem {..} ->
            makeTicklerItem
              authCookieUserUUID
              uuid
              addedItemCreated
              Mergeful.initialServerTime
              addedItemContents
        )
        updates
        (syncRequestTickles $ insertModFunc ups req)
  pure SyncResponse {..}
  where
    updates AddedItem {..} =
      let Tickle {..} = addedItemContents
       in [ TicklerItemType =. itemType tickleContent,
            TicklerItemContents =. itemData tickleContent,
            TicklerItemScheduledDay =. tickleScheduledDay,
            TicklerItemScheduledTime =. tickleScheduledTime,
            TicklerItemRecurrence =. tickleRecurrence
          ]

insertModFunc :: PaidStatus -> SyncRequest -> SyncRequest
insertModFunc ps sr =
  case ps of
    HasNotPaid i ->
      let f cs =
            cs
              { Mergeful.syncRequestNewItems =
                  M.fromList $ take i $ M.toList $ Mergeful.syncRequestNewItems cs
              }
       in sr {syncRequestTickles = f $ syncRequestTickles sr}
    HasPaid _ -> sr
    NoPaymentNecessary -> sr
