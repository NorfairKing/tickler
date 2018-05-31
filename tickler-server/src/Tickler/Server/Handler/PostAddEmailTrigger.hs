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

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostAddEmailTrigger ::
       AuthResult AuthCookie -> AddEmailTrigger -> TicklerHandler TriggerUUID
servePostAddEmailTrigger (Authenticated AuthCookie {..}) AddEmailTrigger {..} = do
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
    pure uuid
servePostAddEmailTrigger _ _ = throwAll err401
