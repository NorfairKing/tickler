{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Types
  ( LooperEnv (..),
    Looper (..),
    runLooper,
    LooperHandle (..),
  )
where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Logger
import Data.Pool
import Database.Persist.Sqlite
import Import
import Tickler.Server.OptParse.Types

data LooperEnv = LooperEnv
  { looperEnvPool :: Pool SqlBackend,
    looperEnvStripeSettings :: Maybe StripeSettings
  }

newtype Looper a = Looper
  { unLooper :: ReaderT LooperEnv (LoggingT IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader LooperEnv,
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

runLooper :: Looper a -> LooperEnv -> LoggingT IO a
runLooper (Looper func) = runReaderT func

data LooperHandle
  = LooperHandleDisabled
  | LooperHandleEnabled (Async ()) LooperStaticConfig
