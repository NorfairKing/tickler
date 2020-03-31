{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.PostEmailTriggerResendVerificationEmail
  ( servePostEmailTriggerResendVerificationEmail
  ) where

import Import

import Data.Time
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostEmailTriggerResendVerificationEmail ::
     AuthCookie -> TriggerUUID -> TicklerHandler NoContent
servePostEmailTriggerResendVerificationEmail AuthCookie {..} tuuid = do
  mt <-
    runDb $
    selectFirst
      [ UserTriggerUserId ==. authCookieUserUUID
      , UserTriggerTriggerType ==. EmailTriggerType
      , UserTriggerTriggerId ==. tuuid
      ]
      []
  case mt of
    Nothing -> throwAll err404 {errBody = "Trigger not found."}
    Just (Entity _ UserTrigger {..}) -> do
      met <- runDb $ selectFirst [EmailTriggerIdentifier ==. tuuid] []
      case met of
        Nothing -> throwAll err404 {errBody = "Email trigger not found."}
        Just (Entity _ EmailTrigger {..}) ->
          if emailTriggerVerified
            then throwAll err400 {errBody = "Email trigger already verified."}
            else do
              now <- liftIO getCurrentTime
              runDb $
                insert_
                  VerificationEmail
                    { verificationEmailTo = emailTriggerAddress
                    , verificationEmailKey = emailTriggerVerificationKey
                    , verificationEmailTrigger = emailTriggerIdentifier
                    , verificationEmailScheduled = now
                    , verificationEmailEmail = Nothing
                    }
              pure NoContent
