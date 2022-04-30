{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PostEmailTrigger (servePostEmailTrigger) where

import Data.Time
import Data.UUID.Typed
import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostEmailTrigger :: AuthCookie -> AddEmailTrigger -> TicklerHandler TriggerUUID
servePostEmailTrigger AuthCookie {..} AddEmailTrigger {..} = do
  now <- liftIO getCurrentTime
  uuid <- liftIO nextRandomUUID
  verificationKey <- liftIO generateRandomVerificationKey
  runDb $ do
    insert_
      EmailTrigger
        { emailTriggerIdentifier = uuid,
          emailTriggerAddress = addEmailTrigger,
          emailTriggerVerificationKey = verificationKey,
          emailTriggerVerified = False,
          emailTriggerAdded = now
        }
    insert_
      UserTrigger
        { userTriggerUserId = authCookieUserUUID,
          userTriggerTriggerType = EmailTriggerType,
          userTriggerTriggerId = uuid
        }
    insert_
      VerificationEmail
        { verificationEmailTo = addEmailTrigger,
          verificationEmailKey = verificationKey,
          verificationEmailTrigger = uuid,
          verificationEmailScheduled = now,
          verificationEmailEmail = Nothing
        }
  pure uuid
