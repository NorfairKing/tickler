{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.PostSync
  ( servePostSync
  ) where

import Import

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Mergeful as Mergeful
import qualified Data.Set as S
import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API
import Tickler.Data

import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

servePostSync :: AuthResult AuthCookie -> SyncRequest -> TicklerHandler SyncResponse
servePostSync (Authenticated AuthCookie {..}) SyncRequest {..} = do
  now <- liftIO getCurrentTime
  let serverSyncProcessorDeleteMany s = do
        runDb $
          deleteWhere
            [TicklerItemUserId ==. authCookieUserUUID, TicklerItemIdentifier <-. S.toList s]
        pure s
      serverSyncProcessorQueryNoLongerSynced s = do
        items <-
          runDb $
          selectList
            [TicklerItemUserId ==. authCookieUserUUID, TicklerItemIdentifier <-. S.toList s]
            []
        let inSButNotInStore =
              s `S.difference` S.fromList (map (ticklerItemIdentifier . entityVal) items)
        pure inSButNotInStore
      serverSyncProcessorQueryNewRemote s =
        M.fromList . map (makeTicklerAddedItem . entityVal) <$>
        runDb
          (selectList
             [TicklerItemUserId ==. authCookieUserUUID, TicklerItemIdentifier /<-. S.toList s]
             [])
      serverSyncProcessorInsertMany ::
           Map Mergeless.ClientId (AddedItem TypedTickle)
        -> TicklerHandler (Map Mergeless.ClientId ItemUUID)
      serverSyncProcessorInsertMany m =
        fmap M.fromList $
        forM (M.toList m) $ \(cid, AddedItem {..}) -> do
          uuid <- nextRandomUUID
          let ii = makeTicklerItem authCookieUserUUID uuid addedItemCreated addedItemContents
          runDb $ insert_ ii
          pure (cid, uuid)
      proc = Mergeless.ServerSyncProcessor {..}
  syncResponseTickles <- Mergeless.processServerSyncCustom proc syncRequestTickles
  pure SyncResponse {..}
servePostSync _ _ = throwAll err401
