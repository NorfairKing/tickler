{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Trigger
  ( makeIntrayTriggerInfo,
    makeEmailTriggerInfo,
  )
where

import Tickler.API

makeIntrayTriggerInfo :: IntrayTrigger -> TriggerInfo
makeIntrayTriggerInfo IntrayTrigger {..} =
  TriggerInfo
    { triggerInfoIdentifier = intrayTriggerIdentifier,
      triggerInfo =
        TriggerIntray
          IntrayTriggerInfo {intrayTriggerInfoUrl = intrayTriggerUrl}
    }

makeEmailTriggerInfo :: EmailTrigger -> TriggerInfo
makeEmailTriggerInfo EmailTrigger {..} =
  TriggerInfo
    { triggerInfoIdentifier = emailTriggerIdentifier,
      triggerInfo =
        TriggerEmail
          EmailTriggerInfo
            { emailTriggerInfoEmailAddress = emailTriggerAddress,
              emailTriggerInfoVerified = emailTriggerVerified
            }
    }
