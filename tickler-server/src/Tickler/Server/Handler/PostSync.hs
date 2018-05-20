{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.PostSync
    ( servePostSync
    ) where

import Import

import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

servePostSync ::
       AuthResult AuthCookie -> SyncRequest -> TicklerHandler SyncResponse
servePostSync (Authenticated AuthCookie {..}) SyncRequest {..} = do
    deleteUndeleted
    -- First we delete the items that were deleted locally but not yet remotely.
    -- Then we find the items that have been deleted remotely but not locally
    deletedRemotely <- syncItemsToBeDeletedLocally
    -- Then we find the items that have appeared remotely but aren't known locally
    newRemoteItems <- syncNewRemoteItems
    -- Then we add the items that should be added.
    newLocalItems <- syncAddedItems
    pure
        SyncResponse
        { syncResponseNewRemoteItems = newRemoteItems
        , syncResponseAddedItems = newLocalItems
        , syncResponseItemsToBeDeletedLocally = deletedRemotely
        }
  where
    deleteUndeleted :: TicklerHandler ()
    deleteUndeleted =
        runDb $
        deleteWhere
            [ TicklerItemUserId ==. authCookieUserUUID
            , TicklerItemIdentifier <-. syncRequestUndeletedItems
            ]
    syncItemsToBeDeletedLocally :: TicklerHandler [ItemUUID]
    syncItemsToBeDeletedLocally = do
        foundItems <-
            runDb $
            selectList
                [ TicklerItemUserId ==. authCookieUserUUID
                , TicklerItemIdentifier <-. syncRequestSyncedItems
                ]
                []
        -- 'foundItems' are the items that HAVEN'T been deleted
        -- So, the items that have been deleted are the ones in 'syncRequestSyncedItems' but not
        -- in 'foundItems'.
        pure $
            syncRequestSyncedItems \\
            map (ticklerItemIdentifier . entityVal) foundItems
    syncNewRemoteItems :: TicklerHandler [ItemInfo TypedItem]
    syncNewRemoteItems =
        map (makeItemInfo . Left . entityVal) <$>
        runDb
            (selectList
                 [ TicklerItemUserId ==. authCookieUserUUID
                 , TicklerItemIdentifier /<-. syncRequestSyncedItems
                 ]
                 [])
    syncAddedItems :: TicklerHandler [ItemInfo TypedItem]
    syncAddedItems = do
        now <- liftIO getCurrentTime
        forM syncRequestUnsyncedItems $ \NewSyncItem {..} -> do
            let ts = fromMaybe now newSyncItemCreated
            uuid <- liftIO nextRandomUUID
            runDb $
                insert_ $
                makeTicklerItem
                    authCookieUserUUID
                    uuid
                    now
                    newSyncItemScheduled
                    newSyncItemContents
            pure
                ItemInfo
                { itemInfoIdentifier = uuid
                , itemInfoCreated = ts
                , itemInfoScheduled = newSyncItemScheduled
                , itemInfoContents = newSyncItemContents
                , itemInfoTriggered = False
                }
servePostSync _ _ = throwAll err401
