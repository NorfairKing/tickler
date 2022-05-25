{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredEmailScheduler
  ( runTriggeredEmailScheduler,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Database.Esqueleto.Legacy
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredEmailScheduler :: Looper ()
runTriggeredEmailScheduler = do
  let source =
        selectSource $
          from $ \((triggeredItem `InnerJoin` user) `CrossJoin` emailTrigger) -> do
            where_ (user ^. UserIdentifier ==. emailTrigger ^. EmailTriggerUser)
            where_ (emailTrigger ^. EmailTriggerVerified ==. val True)
            on (triggeredItem ^. TriggeredItemUserId ==. user ^. UserIdentifier)
            where_ $
              notExists $
                from $ \triggeredEmail ->
                  where_ $
                    (triggeredEmail ^. TriggeredEmailTrigger ==. emailTrigger ^. EmailTriggerIdentifier)
                      &&. (triggeredEmail ^. TriggeredEmailItem ==. triggeredItem ^. TriggeredItemIdentifier)
            pure (triggeredItem, emailTrigger)

  runDb $ runConduit $ source .| C.mapM_ (uncurry scheduleTriggeredEmailWithEmailTrigger)

scheduleTriggeredEmailWithEmailTrigger :: (MonadIO m, MonadLogger m) => Entity TriggeredItem -> Entity EmailTrigger -> SqlPersistT m ()
scheduleTriggeredEmailWithEmailTrigger (Entity _ ti) (Entity _ EmailTrigger {..}) = do
  logInfoN $
    T.pack $
      unwords
        [ "Scheduling a triggered email item for triggered item with identifier",
          uuidString $ triggeredItemIdentifier ti
        ]
  insert_
    TriggeredEmail
      { triggeredEmailItem = triggeredItemIdentifier ti,
        triggeredEmailTrigger = emailTriggerIdentifier,
        triggeredEmailEmail = Nothing,
        triggeredEmailError = Nothing
      }
