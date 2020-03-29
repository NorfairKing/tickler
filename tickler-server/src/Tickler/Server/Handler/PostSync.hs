{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.PostSync
  ( servePostSync
  ) where

import Import

import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import qualified Data.Mergeful.Timed as Mergeful
import Data.UUID.Typed

import Database.Persist
import Database.Persist.Sqlite

import Tickler.API

import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

servePostSync :: AuthCookie -> SyncRequest -> TicklerHandler SyncResponse
servePostSync AuthCookie {..} req = do
  ups <- getUserPaidStatus authCookieUserUUID
  runDb $ do
    serverStore <- readServerStore authCookieUserUUID
    (resp, serverStore') <-
      Mergeful.processServerSync
        nextRandomUUID
        serverStore
        (syncRequestTickles $ insertModFunc ups req)
    writeServerStore authCookieUserUUID serverStore'
    pure (SyncResponse {syncResponseTickles = resp})

type ServerStore = Mergeful.ServerStore ItemUUID (AddedItem TypedTickle)

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

readServerStore :: AccountUUID -> SqlPersistT IO ServerStore
readServerStore u =
  Mergeful.ServerStore . M.fromList . map go <$> selectList [TicklerItemUserId ==. u] []
  where
    go :: Entity TicklerItem -> (ItemUUID, Mergeful.Timed (AddedItem TypedTickle))
    go (Entity _ ti) = makeTicklerAdded ti

writeServerStore :: AccountUUID -> ServerStore -> SqlPersistT IO ()
writeServerStore u ss = do
  deleteWhere [TicklerItemUserId ==. u] -- Clean slate
  forM_ (M.toList $ Mergeful.serverStoreItems ss) $ \(uuid, Mergeful.Timed (AddedItem tt ct) st) ->
    let ti@TicklerItem {..} = makeTicklerItem u uuid ct st tt
     in upsertBy
          (UniqueItemIdentifier uuid)
          ti
          [ TicklerItemType =. ticklerItemType
          , TicklerItemContents =. ticklerItemContents
          , TicklerItemServerTime =. st
          ]
