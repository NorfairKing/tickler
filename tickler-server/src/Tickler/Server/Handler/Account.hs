{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Account
  ( getUserAccountInfo
  ) where

import Import

import Database.Persist

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils

getUserAccountInfo :: User -> TicklerHandler AccountInfo
getUserAccountInfo User {..} = do
  admins <- asks envAdmins
  ticklerItemCount <- runDb $ count [TicklerItemUserId ==. userIdentifier]
  triggeredItemCount <- runDb $ count [TriggeredItemUserId ==. userIdentifier]
  ups <- getUserPaidStatus userIdentifier
  pure
    AccountInfo
      { accountInfoUUID = userIdentifier
      , accountInfoUsername = userUsername
      , accountInfoCreated = userCreated
      , accountInfoLastLogin = userLastLogin
      , accountInfoAdmin = userUsername `elem` admins
      , accountInfoTicklerItemCount = ticklerItemCount
      , accountInfoTriggeredItemCount = triggeredItemCount
      , accountInfoStatus = ups
      }
