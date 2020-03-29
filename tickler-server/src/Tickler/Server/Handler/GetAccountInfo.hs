{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetAccountInfo
  ( serveGetAccountInfo
  , getUserAccountInfo
  ) where

import Import

import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils

serveGetAccountInfo :: AuthCookie -> TicklerHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ u) -> getUserAccountInfo u

getUserAccountInfo :: User -> TicklerHandler AccountInfo
getUserAccountInfo User {..} = do
  admins <- asks envAdmins
  ticklerItemCount <- runDb $ count [TicklerItemUserId ==. userIdentifier]
  triggeredItemCount <- runDb $ count [TriggeredItemUserId ==. userIdentifier]
  ups <- getUserPaidStatus userIdentifier
  let subbed =
        case ups of
          HasNotPaid _ -> Nothing
          HasPaid u -> Just u
          NoPaymentNecessary -> Nothing
  pure
    AccountInfo
      { accountInfoUUID = userIdentifier
      , accountInfoUsername = userUsername
      , accountInfoCreated = userCreated
      , accountInfoLastLogin = userLastLogin
      , accountInfoAdmin = userUsername `elem` admins
      , accountInfoTicklerItemCount = ticklerItemCount
      , accountInfoTriggeredItemCount = triggeredItemCount
      , accountInfoSubscribed = subbed
      }
