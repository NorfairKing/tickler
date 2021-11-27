{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Trigger
  ( makeIntrayTriggerInfo,
    makeEmailTriggerInfo,
  )
where

import Data.Aeson as JSON
import Import
import Tickler.API

makeIntrayTriggerInfo :: IntrayTrigger -> TriggerInfo TypedTriggerInfo
makeIntrayTriggerInfo IntrayTrigger {..} =
  TriggerInfo
    { triggerInfoIdentifier = intrayTriggerIdentifier,
      triggerInfo =
        TypedTriggerInfo
          { typedTriggerInfoType = IntrayTriggerType,
            typedTriggerInfoValue =
              toJSON $ IntrayTriggerInfo {intrayTriggerInfoUrl = intrayTriggerUrl}
          }
    }

makeEmailTriggerInfo :: EmailTrigger -> TriggerInfo TypedTriggerInfo
makeEmailTriggerInfo EmailTrigger {..} =
  TriggerInfo
    { triggerInfoIdentifier = emailTriggerIdentifier,
      triggerInfo =
        TypedTriggerInfo
          { typedTriggerInfoType = EmailTriggerType,
            typedTriggerInfoValue =
              toJSON $
                EmailTriggerInfo
                  { emailTriggerInfoEmailAddress = emailTriggerAddress,
                    emailTriggerInfoVerified = emailTriggerVerified
                  }
          }
    }
