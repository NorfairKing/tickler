{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Utils
  ( runDb,
    withAdminCreds,
    sendAdminNotification,
    deleteAccountFully,
  )
where

import Database.Esqueleto.Legacy ((^.))
import qualified Database.Esqueleto.Legacy as E
import Database.Persist
import Database.Persist.Sqlite
import Import
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

sendAdminNotification :: MonadIO m => Text -> SqlPersistT m ()
sendAdminNotification contents =
  insert_
    AdminNotificationEmail
      { adminNotificationEmailContents = contents,
        adminNotificationEmailEmail = Nothing
      }

deleteAccountFully :: AccountUUID -> TicklerHandler ()
deleteAccountFully uuid = do
  mEnt <- runDb $ getBy $ UniqueUserIdentifier uuid
  case mEnt of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity uid _) ->
      runDb $ do
        deleteWhere [TicklerItemUserId ==. uuid]
        deleteWhere [TriggeredItemUserId ==. uuid]

        E.delete $
          E.from $ \triggeredIntrayItem ->
            E.where_ $
              E.exists $
                E.from $ \intrayTrigger ->
                  E.where_ $
                    (intrayTrigger ^. IntrayTriggerIdentifier E.==. triggeredIntrayItem ^. TriggeredIntrayItemTrigger)
                      E.&&. (intrayTrigger ^. IntrayTriggerUser E.==. E.val uuid)

        E.delete $
          E.from $ \intrayTrigger ->
            E.where_
              (intrayTrigger ^. IntrayTriggerUser E.==. E.val uuid)

        E.delete $
          E.from $ \verificationEmail ->
            E.where_ $
              E.exists $
                E.from $ \emailTrigger ->
                  E.where_ $
                    (emailTrigger ^. EmailTriggerIdentifier E.==. verificationEmail ^. VerificationEmailTrigger)
                      E.&&. (emailTrigger ^. EmailTriggerUser E.==. E.val uuid)

        E.delete $
          E.from $ \triggeredEmail ->
            E.where_ $
              E.exists $
                E.from $ \emailTrigger ->
                  E.where_ $
                    (emailTrigger ^. EmailTriggerIdentifier E.==. triggeredEmail ^. TriggeredEmailTrigger)
                      E.&&. (emailTrigger ^. EmailTriggerUser E.==. E.val uuid)

        E.delete $
          E.from $ \emailTrigger ->
            E.where_
              (emailTrigger ^. EmailTriggerUser E.==. E.val uuid)

        delete uid
