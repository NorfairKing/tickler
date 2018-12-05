{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server.Handler.PostAddIntrayTrigger
    ( servePostAddIntrayTrigger
    ) where

import Import

import qualified Data.Text as T
import Data.Time
import Data.UUID.Typed
import Database.Persist
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client

import qualified Intray.Client as Intray

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostAddIntrayTrigger ::
       AuthResult AuthCookie
    -> AddIntrayTrigger
    -> TicklerHandler (Either Text TriggerUUID)
servePostAddIntrayTrigger (Authenticated AuthCookie {..}) AddIntrayTrigger {..} = do
    now <- liftIO getCurrentTime
    uuid <- liftIO nextRandomUUID
    man <- liftIO $ Http.newManager Http.tlsManagerSettings
    errOrOk <-
        do let env = ClientEnv man addIntrayTriggerUrl Nothing
           liftIO $
               flip runClientM env $ do
                   let loginForm =
                           Intray.LoginForm
                               { Intray.loginFormUsername =
                                     addIntrayTriggerUsername
                               , Intray.loginFormPassword =
                                     Intray.accessKeySecretText
                                         addIntrayTriggerAccessKey
                               }
                   Headers Intray.NoContent (HCons _ (HCons _ HNil)) <-
                       Intray.clientPostLogin loginForm
                   pure ()
    case errOrOk of
        Left err ->
            case err of
                ConnectionError t -> pure $ Left t
                _ -> pure $ Left $ T.pack $ ppShow err
        Right () -> do
            runDb $ do
                insert_
                    IntrayTrigger
                        { intrayTriggerIdentifier = uuid
                        , intrayTriggerUrl = addIntrayTriggerUrl
                        , intrayTriggerUsername = addIntrayTriggerUsername
                        , intrayTriggerAccessKey = addIntrayTriggerAccessKey
                        , intrayTriggerAdded = now
                        }
                insert_
                    UserTrigger
                        { userTriggerUserId = authCookieUserUUID
                        , userTriggerTriggerType = IntrayTriggerType
                        , userTriggerTriggerId = uuid
                        }
            pure $ Right uuid
servePostAddIntrayTrigger _ _ = throwAll err401
