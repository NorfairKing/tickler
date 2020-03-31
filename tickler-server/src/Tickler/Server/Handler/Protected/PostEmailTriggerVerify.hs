{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.PostEmailTriggerVerify
  ( servePostEmailTriggerVerify
  ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostEmailTriggerVerify ::
     AuthCookie -> TriggerUUID -> EmailVerificationKey -> TicklerHandler NoContent
servePostEmailTriggerVerify AuthCookie {..} tuuid evk = do
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
        Just (Entity etid EmailTrigger {..}) ->
          if emailTriggerVerificationKey == evk
            then runDb $ update etid [EmailTriggerVerified =. True]
            else throwAll err400 {errBody = "Incorrect verification key."}
  pure NoContent
