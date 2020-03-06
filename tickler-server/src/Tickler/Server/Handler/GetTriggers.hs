{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetTriggers where

import Import

import Data.Aeson as JSON
import Database.Persist

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetTriggers :: AuthCookie -> TicklerHandler [TriggerInfo TypedTriggerInfo]
serveGetTriggers AuthCookie {..} = do
  uts <- runDb $ selectList [UserTriggerUserId ==. authCookieUserUUID] []
  runDb $
    fmap catMaybes $
    forM uts $ \(Entity _ UserTrigger {..}) ->
      liftA2
        mplus
        (fmap (makeIntrayTriggerInfo . entityVal) <$>
         selectFirst [IntrayTriggerIdentifier ==. userTriggerTriggerId] [])
        (fmap (makeEmailTriggerInfo . entityVal) <$>
         selectFirst [EmailTriggerIdentifier ==. userTriggerTriggerId] [])

makeIntrayTriggerInfo :: IntrayTrigger -> TriggerInfo TypedTriggerInfo
makeIntrayTriggerInfo IntrayTrigger {..} =
  TriggerInfo
    { triggerInfoIdentifier = intrayTriggerIdentifier
    , triggerInfo =
        TypedTriggerInfo
          { typedTriggerInfoType = IntrayTriggerType
          , typedTriggerInfoValue =
              toJSON $ IntrayTriggerInfo {intrayTriggerInfoUrl = intrayTriggerUrl}
          }
    }

makeEmailTriggerInfo :: EmailTrigger -> TriggerInfo TypedTriggerInfo
makeEmailTriggerInfo EmailTrigger {..} =
  TriggerInfo
    { triggerInfoIdentifier = emailTriggerIdentifier
    , triggerInfo =
        TypedTriggerInfo
          { typedTriggerInfoType = EmailTriggerType
          , typedTriggerInfoValue =
              toJSON $
              EmailTriggerInfo
                { emailTriggerInfoEmailAddress = emailTriggerAddress
                , emailTriggerInfoVerified = emailTriggerVerified
                }
          }
    }
