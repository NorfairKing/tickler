{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server.Handler.Protected.PostAddIntrayTrigger
  ( servePostAddIntrayTrigger,
  )
where

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

servePostAddIntrayTrigger ::
  AuthCookie -> AddIntrayTrigger -> TicklerHandler (Either Text TriggerUUID)
servePostAddIntrayTrigger AuthCookie {..} AddIntrayTrigger {..} = do
  now <- liftIO getCurrentTime
  uuid <- liftIO nextRandomUUID
  man <- liftIO $ Http.newManager Http.tlsManagerSettings
  errOrOk <-
    do
      let env = ClientEnv man addIntrayTriggerUrl Nothing
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
        ConnectionError t -> pure $ Left (T.pack (show t))
        _ -> pure $ Left $ T.pack $ ppShow err
    Right () -> do
      runDb $ do
        insert_
          IntrayTrigger
            { intrayTriggerIdentifier = uuid,
              intrayTriggerUrl = addIntrayTriggerUrl,
              intrayTriggerUsername = addIntrayTriggerUsername,
              intrayTriggerAccessKey = addIntrayTriggerAccessKey,
              intrayTriggerAdded = now
            }
        insert_
          UserTrigger
            { userTriggerUserId = authCookieUserUUID,
              userTriggerTriggerType = IntrayTriggerType,
              userTriggerTriggerId = uuid
            }
      pure $ Right uuid
