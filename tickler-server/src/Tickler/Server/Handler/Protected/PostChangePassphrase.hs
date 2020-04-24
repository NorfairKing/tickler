{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server.Handler.Protected.PostChangePassphrase
  ( servePostChangePassphrase
  ) where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostChangePassphrase :: AuthCookie -> ChangePassphrase -> TicklerHandler NoContent
servePostChangePassphrase AuthCookie {..} ChangePassphrase {..} = do
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just (Entity uid User {..}) ->
      if validatePassword userHashedPassword changePassphraseOld
        then do
          mhp <- liftIO $ passwordHash changePassphraseNew
          case mhp of
            Nothing -> throwError $ err500 {errBody = "Unable to hash new password."}
            Just hp -> do
              runDb $ update uid [UserHashedPassword =. hp]
              pure NoContent
        else throwError $ err403 {errBody = "Old password does not match."}
