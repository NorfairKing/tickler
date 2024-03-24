{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PostEmailTriggerResendVerificationEmail
  ( servePostEmailTriggerResendVerificationEmail,
  )
where

import Data.Time
import Database.Persist
import Import
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostEmailTriggerResendVerificationEmail ::
  AuthCookie -> TriggerUUID -> TicklerHandler NoContent
servePostEmailTriggerResendVerificationEmail AuthCookie {..} tuuid = do
  met <- runDB $ selectFirst [EmailTriggerUser ==. authCookieUserUUID, EmailTriggerIdentifier ==. tuuid] []
  case met of
    Nothing -> throwAll err404
    Just (Entity _ EmailTrigger {..}) -> do
      if emailTriggerVerified
        then throwAll err409 {errBody = "Email trigger already verified."}
        else do
          emailsAlreadyQueued <- runDB $ count [VerificationEmailTrigger ==. tuuid, VerificationEmailEmail ==. Nothing]
          if emailsAlreadyQueued >= 1
            then throwAll err400 {errBody = "Verification email already scheduled."}
            else do
              now <- liftIO getCurrentTime
              runDB
                $ insert_
                  VerificationEmail
                    { verificationEmailTo = emailTriggerAddress,
                      verificationEmailKey = emailTriggerVerificationKey,
                      verificationEmailTrigger = emailTriggerIdentifier,
                      verificationEmailScheduled = now,
                      verificationEmailEmail = Nothing
                    }
              pure NoContent
