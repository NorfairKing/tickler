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

import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Time
import Data.UUID.Typed
import Database.Persist
import qualified Network.HTTP.Client as Http

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Client
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client

import qualified Intray.Client as Intray

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostAddIntrayTrigger ::
       AuthResult AuthCookie -> AddIntrayTrigger -> TicklerHandler TriggerUUID
servePostAddIntrayTrigger (Authenticated AuthCookie {..}) AddIntrayTrigger {..} = do
    now <- liftIO getCurrentTime
    uuid <- liftIO nextRandomUUID
    man <- liftIO $ Http.newManager Http.defaultManagerSettings
    errOrOk <-
        do let env = ClientEnv man addIntrayTriggerUrl Nothing
           case Intray.parseUsername addIntrayTriggerUsername of
               Nothing -> pure $ Left "Invalid Intray username"
               Just un ->
                   liftIO $
                   fmap (left show) $
                   flip runClientM env $ do
                       let loginForm =
                               Intray.LoginForm
                               { Intray.loginFormUsername = un
                               , Intray.loginFormPassword =
                                     addIntrayTriggerAccessKey
                               }
                       Headers Intray.NoContent (HCons _ (HCons sessionHeader HNil)) <-
                           Intray.clientPostLogin loginForm
                       pure ()
    case errOrOk of
        Left err ->
            throwAll
                err400
                { errBody =
                      "Failed to login to the intray instance with the given credentials:" <>
                      LB8.pack err
                }
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
            pure uuid
servePostAddIntrayTrigger _ _ = throwAll err401
