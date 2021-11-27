{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.GetAccountInfo
  ( serveGetAccountInfo,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Account
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveGetAccountInfo :: AuthCookie -> TicklerHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ u) -> getUserAccountInfo u
