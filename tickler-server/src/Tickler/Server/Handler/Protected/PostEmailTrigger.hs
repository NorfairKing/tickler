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
  runDB $ do
    insert_
      EmailTrigger
        { emailTriggerUser = authCookieUserUUID,
          emailTriggerIdentifier = uuid,
          emailTriggerAddress = addEmailTriggerEmailAddress,
          emailTriggerVerificationKey = verificationKey,
          emailTriggerVerified = False,
          emailTriggerAdded = now
        }
    insert_
      VerificationEmail
        { verificationEmailTo = addEmailTriggerEmailAddress,
          verificationEmailKey = verificationKey,
          verificationEmailTrigger = uuid,
          verificationEmailScheduled = now,
          verificationEmailEmail = Nothing
        }
  pure uuid
