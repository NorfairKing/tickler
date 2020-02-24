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

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API
import Tickler.Data

import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

servePostSync :: AuthResult AuthCookie -> SyncRequest -> TicklerHandler SyncResponse
servePostSync (Authenticated AuthCookie {..}) req =
  runDb $ do
    serverStore <- readServerStore authCookieUserUUID
    (resp, serverStore') <-
      Mergeful.processServerSync nextRandomUUID serverStore (syncRequestTickles req)
    writeServerStore authCookieUserUUID serverStore'
    pure (SyncResponse {syncResponseTickles = resp})
servePostSync _ _ = throwAll err401

type ServerStore = Mergeful.ServerStore ItemUUID (AddedItem TypedTickle)

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
