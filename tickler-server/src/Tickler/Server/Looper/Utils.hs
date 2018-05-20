{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Utils
    ( runDb
    ) where

import Import

import Control.Concurrent

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool
import Data.Time
import Database.Persist.Sqlite

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types

runDb :: SqlPersistT IO b -> Looper b
runDb query = do
    pool <- ask
    liftIO $ runSqlPool query pool
