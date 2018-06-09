{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Types
    ( Looper(..)
    , runLooper
    , LooperHandle(..)
    ) where

import Import

import Control.Concurrent.Async
import Control.Monad.Logger
import Data.Pool
import Database.Persist.Sqlite

newtype Looper a = Looper
    { unLooper :: ReaderT (Pool SqlBackend) (LoggingT IO) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader (Pool SqlBackend)
               , MonadLogger
               )

runLooper :: Looper a -> Pool SqlBackend -> IO a
runLooper (Looper func) = runStderrLoggingT . runReaderT func

data LooperHandle
    = LooperHandleDisabled
    | LooperHandleEnabled (Async ())
