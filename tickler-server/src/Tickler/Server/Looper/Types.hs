{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Types
  ( LooperEnv(..)
  , Looper(..)
  , LooperHandle(..)
  ) where

import Import

import Looper

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Logger
import Data.Pool
import Database.Persist.Sqlite
import Tickler.Server.OptParse.Types

newtype LooperEnv =
  LooperEnv
    { looperEnvPool :: Pool SqlBackend
    }

type Looper = ReaderT LooperEnv (LoggingT IO)

data LooperHandle
  = LooperHandleDisabled
  | LooperHandleEnabled (Async ()) LooperSettings
