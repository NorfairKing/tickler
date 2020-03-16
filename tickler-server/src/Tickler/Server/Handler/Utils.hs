{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Utils
  ( runDb
  , withAdminCreds
  , deleteAccountFully
  ) where

import Import

import Database.Persist
import Database.Persist.Sqlite

import Servant
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

runDb :: (MonadReader TicklerServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks envConnectionPool
  liftIO $ runSqlPool query pool

withAdminCreds :: AccountUUID -> TicklerHandler a -> TicklerHandler a
withAdminCreds adminCandidate func = do
  admins <- asks envAdmins
  mUser <- runDb $ getBy $ UniqueUserIdentifier adminCandidate
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ User {..}) ->
      if userUsername `elem` admins
        then func
        else throwAll err401

deleteAccountFully :: AccountUUID -> TicklerHandler ()
deleteAccountFully uuid = do
  mEnt <- runDb $ getBy $ UniqueUserIdentifier uuid
  case mEnt of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity uid _) ->
      runDb $ do
        deleteWhere [TicklerItemUserId ==. uuid]
        deleteWhere [TriggeredItemUserId ==. uuid]
        ets <-
          map (userTriggerTriggerId . entityVal) <$> selectList [UserTriggerUserId ==. uuid] [] -- TODO this needs a join
        deleteWhere [UserTriggerUserId ==. uuid]
        deleteWhere [IntrayTriggerIdentifier <-. ets]
        deleteWhere [EmailTriggerIdentifier <-. ets]
        deleteWhere [VerificationEmailTrigger <-. ets]
        deleteWhere [TriggeredIntrayItemTrigger <-. ets]
        deleteWhere [TriggeredEmailTrigger <-. ets]
        delete uid
