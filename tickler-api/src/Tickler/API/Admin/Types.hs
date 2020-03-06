{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.API.Admin.Types where

import Import

import Data.Aeson as JSON

import Servant.Docs

import Tickler.API.Types ()

data AdminStats =
  AdminStats
    { adminStatsNbUsers :: Int
    , adminStatsNbItems :: Int
    }
  deriving (Show, Eq, Ord, Generic)

instance Validity AdminStats

instance FromJSON AdminStats where
  parseJSON = withObject "AdminStats" $ \o -> AdminStats <$> o .: "users" <*> o .: "items"

instance ToJSON AdminStats where
  toJSON AdminStats {..} = object ["users" .= adminStatsNbUsers, "items" .= adminStatsNbItems]

instance ToSample AdminStats
