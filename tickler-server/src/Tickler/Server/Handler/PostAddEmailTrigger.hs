{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.PostAddEmailTrigger
  ( servePostAddEmailTrigger
  ) where

import Import

import Data.Time
import Data.UUID.Typed
import Database.Persist

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostAddEmailTrigger :: AuthCookie -> AddEmailTrigger -> TicklerHandler TriggerUUID
servePostAddEmailTrigger AuthCookie {..} AddEmailTrigger {..} = do
  now <- liftIO getCurrentTime
  uuid <- liftIO nextRandomUUID
  verificationKey <- liftIO generateRandomVerificationKey
  runDb $ do
    insert_
      EmailTrigger
        { emailTriggerIdentifier = uuid
        , emailTriggerAddress = addEmailTrigger
        , emailTriggerVerificationKey = verificationKey
        , emailTriggerVerified = False
        , emailTriggerAdded = now
        }
    insert_
      UserTrigger
        { userTriggerUserId = authCookieUserUUID
        , userTriggerTriggerType = EmailTriggerType
        , userTriggerTriggerId = uuid
        }
    insert_
      VerificationEmail
        { verificationEmailTo = addEmailTrigger
        , verificationEmailKey = verificationKey
        , verificationEmailTrigger = uuid
        , verificationEmailScheduled = now
        , verificationEmailEmail = Nothing
        }
  pure uuid
