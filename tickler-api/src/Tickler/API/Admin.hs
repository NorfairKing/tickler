{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Admin where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Import
import Servant.API
import Servant.API.Generic
import Tickler.API.Account
import Tickler.API.Types
import Tickler.Data

type TicklerAdminAPI = ToServantApi TicklerAdminSite

data TicklerAdminSite route = TicklerAdminSite
  { adminGetStats :: route :- AdminGetStats,
    adminDeleteAccount :: route :- AdminDeleteAccount,
    adminGetAccounts :: route :- AdminGetAccounts
  }
  deriving (Generic)

type AdminGetStats =
  ProtectAPI
    :> "stats"
    :> Get '[JSON] AdminStats

data AdminStats = AdminStats
  { adminStatsNbUsers :: Word,
    adminStatsNbSubscribers :: Word,
    adminStatsNbTicklerItems :: Word,
    adminStatsNbTriggeredItems :: Word,
    adminStatsActiveUsers :: !ActiveUsers
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AdminStats)

instance Validity AdminStats

instance HasCodec AdminStats where
  codec =
    object "AdminStats" $
      AdminStats
        <$> requiredField "users" "number of users" .= adminStatsNbUsers
        <*> requiredField "subscribers" "number of subscribers" .= adminStatsNbSubscribers
        <*> requiredField "tickler-items" "number of tickler items" .= adminStatsNbTicklerItems
        <*> requiredField "triggered-items" "number of triggered items" .= adminStatsNbTriggeredItems
        <*> requiredField "active-users" "active users" .= adminStatsActiveUsers

data ActiveUsers = ActiveUsers
  { activeUsersDaily :: !Word,
    activeUsersWeekly :: !Word,
    activeUsersMonthly :: !Word,
    activeUsersYearly :: !Word
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ActiveUsers)

instance Validity ActiveUsers

instance HasCodec ActiveUsers where
  codec =
    object "ActiveUsers" $
      ActiveUsers
        <$> requiredField "daily" "daily active users" .= activeUsersDaily
        <*> requiredField "weekly" "weekly active users" .= activeUsersWeekly
        <*> requiredField "monthly" "monthly active users" .= activeUsersMonthly
        <*> requiredField "yearly" "yearly active users" .= activeUsersYearly

type AdminDeleteAccount =
  ProtectAPI
    :> "account"
    :> Capture "id" AccountUUID
    :> Delete '[JSON] NoContent

type AdminGetAccounts =
  ProtectAPI
    :> "accounts"
    :> Get '[JSON] [AccountInfo]
