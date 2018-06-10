{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.PostSync
    ( servePostSync
    ) where

import Import

import Data.Mergeless
import qualified Data.Set as S
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
       AuthResult AuthCookie
    -> SyncRequest ItemUUID TypedItem
    -> TicklerHandler (SyncResponse ItemUUID TypedItem)
servePostSync (Authenticated AuthCookie {..}) sr = do
    now <- liftIO getCurrentTime
    let syncProcessorDeleteMany s =
            runDb $
            deleteWhere
                [ TicklerItemUserId ==. authCookieUserUUID
                , TicklerItemIdentifier <-. S.toList s
                ]
        syncProcessorQuerySynced s =
            S.fromList . map (ticklerItemIdentifier . entityVal) <$>
            runDb
                (selectList
                     [ TicklerItemUserId ==. authCookieUserUUID
                     , TicklerItemIdentifier <-. S.toList s
                     ]
                     [])
        syncProcessorQueryNewRemote s =
            S.fromList . map (makeTicklerSynced . entityVal) <$>
            runDb
                (selectList
                     [ TicklerItemUserId ==. authCookieUserUUID
                     , TicklerItemIdentifier /<-. S.toList s
                     ]
                     [])
        syncProcessorInsertMany s =
            runDb $
            insertMany_ $
            flip map (S.toList s) $ \Synced {..} ->
                makeTicklerItem authCookieUserUUID syncedUuid syncedCreated syncedSynced syncedValue
        proc = SyncProcessor {..}
    processSyncCustom nextRandomUUID now proc sr
servePostSync _ _ = throwAll err401
