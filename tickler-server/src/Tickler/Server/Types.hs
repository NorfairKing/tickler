module Tickler.Server.Types where

import Database.Persist.Sqlite
import Import
import Servant
import Servant.Auth.Server
import Tickler.API
import Tickler.Server.OptParse.Types

type LF = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

data TicklerServerEnv = TicklerServerEnv
  { envLogFunc :: !LF,
    envConnectionPool :: !ConnectionPool,
    envCookieSettings :: !CookieSettings,
    envJWTSettings :: !JWTSettings,
    envAdmins :: ![Username],
    envFreeloaders :: ![Username],
    envMonetisation :: !(Maybe MonetisationSettings)
  }

type TicklerHandler = ReaderT TicklerServerEnv (LoggingT Handler)
