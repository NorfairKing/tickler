{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PostEmailTriggerVerify (servePostEmailTriggerVerify) where

import Database.Persist
import Import
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostEmailTriggerVerify ::
  AuthCookie -> TriggerUUID -> EmailVerificationKey -> TicklerHandler NoContent
servePostEmailTriggerVerify AuthCookie {..} tuuid evk = do
  mt <-
    runDb $
      selectFirst
        [ EmailTriggerUser ==. Just authCookieUserUUID,
          EmailTriggerIdentifier ==. tuuid
        ]
        []
  case mt of
    Nothing -> throwAll err404
    Just (Entity etid EmailTrigger {..}) ->
      if emailTriggerVerificationKey == evk
        then runDb $ update etid [EmailTriggerVerified =. True]
        else throwAll err400 {errBody = "Incorrect verification key."}
  pure NoContent
