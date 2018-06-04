{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.UUID
    (
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.UUID.Typed

import Database.Persist
import Database.Persist.Sql

import Intray.Data
