{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Trigger
  ( makeIntrayTriggerInfo
  , makeEmailTriggerInfo
  ) where

import Import

import Data.Aeson as JSON

import Tickler.API

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
