{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Types
  ( LooperEnv(..)
  , Looper(..)
  , runLooper
  , LooperHandle(..)
  ) where

import Import

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

newtype Looper a =
  Looper
    { unLooper :: ReaderT LooperEnv (LoggingT IO) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader LooperEnv
           , MonadLogger
           , MonadThrow
           , MonadCatch
           , MonadMask
           )

runLooper :: Looper a -> LooperEnv -> IO a
runLooper (Looper func) = runStderrLoggingT . runReaderT func

data LooperHandle
  = LooperHandleDisabled
  | LooperHandleEnabled (Async ()) LooperStaticConfig
