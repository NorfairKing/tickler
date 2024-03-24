{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PostIntrayTrigger (servePostIntrayTrigger) where

import qualified Data.Text as T
import Data.Time
import Data.UUID.Typed
import Database.Persist
import Import
import qualified Intray.Client as Intray
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant hiding (BadPassword, NoSuchUser)
import Servant.Client
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostIntrayTrigger ::
  AuthCookie -> AddIntrayTrigger -> TicklerHandler (Either Text TriggerUUID)
servePostIntrayTrigger AuthCookie {..} AddIntrayTrigger {..} = do
  now <- liftIO getCurrentTime
  uuid <- liftIO nextRandomUUID
  man <- liftIO $ Http.newManager Http.tlsManagerSettings
  errOrOk <-
    do
      let env = mkClientEnv man addIntrayTriggerUrl
      liftIO
        $ flip runClientM env
        $ do
          let loginForm =
                Intray.LoginForm
                  { Intray.loginFormUsername = addIntrayTriggerUsername,
                    Intray.loginFormPassword = Intray.accessKeySecretText addIntrayTriggerAccessKey
                  }
          res <- Intray.clientPostLogin loginForm
          case res of
            Headers Intray.NoContent (HCons _ HNil) -> pure ()
  case errOrOk of
    Left err ->
      case err of
        ConnectionError t -> pure $ Left (T.pack (ppShow t))
        FailureResponse req resp -> pure $ Left $ T.pack $ unlines [ppShow req, ppShow resp]
        _ -> pure $ Left $ T.pack $ ppShow err
    Right () -> do
      runDB
        $ insert_
          IntrayTrigger
            { intrayTriggerUser = authCookieUserUUID,
              intrayTriggerIdentifier = uuid,
              intrayTriggerUrl = addIntrayTriggerUrl,
              intrayTriggerUsername = addIntrayTriggerUsername,
              intrayTriggerAccessKey = addIntrayTriggerAccessKey,
              intrayTriggerAdded = now
            }
      pure $ Right uuid
