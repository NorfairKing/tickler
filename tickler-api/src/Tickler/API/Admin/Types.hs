{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.API.Admin.Types where

import Data.Aeson as JSON
import Import
import Servant.Docs
import Tickler.API.Types ()

data AdminStats
  = AdminStats
      { adminStatsNbUsers :: Word,
        adminStatsNbSubscribers :: Word,
        adminStatsNbTicklerItems :: Word,
        adminStatsNbTriggeredItems :: Word,
        adminStatsActiveUsers :: !ActiveUsers
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity AdminStats

instance FromJSON AdminStats where
  parseJSON =
    withObject "AdminStats" $ \o ->
      AdminStats <$> o .: "users" <*> o .: "subscribers" <*> o .: "tickler-items"
        <*> o .: "triggered-items"
        <*> o .: "active-users"

instance ToJSON AdminStats where
  toJSON AdminStats {..} =
    object
      [ "users" .= adminStatsNbUsers,
        "subscribers" .= adminStatsNbSubscribers,
        "tickler-items" .= adminStatsNbTicklerItems,
        "triggered-items" .= adminStatsNbTriggeredItems,
        "active-users" .= adminStatsActiveUsers
      ]

instance ToSample AdminStats

data ActiveUsers
  = ActiveUsers
      { activeUsersDaily :: !Word,
        activeUsersWeekly :: !Word,
        activeUsersMonthly :: !Word,
        activeUsersYearly :: !Word
      }
  deriving (Show, Eq, Ord, Generic)

instance Validity ActiveUsers

instance FromJSON ActiveUsers where
  parseJSON =
    withObject "ActiveUsers" $ \o ->
      ActiveUsers <$> o .: "daily" <*> o .: "weekly" <*> o .: "monthly" <*> o .: "yearly"

instance ToJSON ActiveUsers where
  toJSON ActiveUsers {..} =
    object
      [ "daily" .= activeUsersDaily,
        "weekly" .= activeUsersWeekly,
        "monthly" .= activeUsersMonthly,
        "yearly" .= activeUsersYearly
      ]

instance ToSample ActiveUsers
