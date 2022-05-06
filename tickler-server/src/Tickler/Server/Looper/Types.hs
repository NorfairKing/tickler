{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Types
  ( LooperEnv (..),
    Looper (..),
    runLooper,
  )
where

import Control.Monad.Catch
import Control.Monad.Logger
import Data.Pool
import Database.Persist.Sqlite
import Import
import Tickler.Server.OptParse.Types
import UnliftIO

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
      MonadUnliftIO,
      MonadReader LooperEnv,
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

runLooper :: Looper a -> LooperEnv -> LoggingT IO a
runLooper (Looper func) = runReaderT func
