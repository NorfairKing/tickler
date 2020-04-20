{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Public.PostLogin
  ( servePostLogin
  ) where

import Import

import Control.Monad.Except
import qualified Data.Text.Encoding as TE
import Data.Time
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostLogin :: LoginForm -> TicklerHandler (Headers '[ Header "Set-Cookie" Text] NoContent)
servePostLogin LoginForm {..} = do
  me <- runDb $ getBy $ UniqueUsername loginFormUsername
  case me of
    Nothing -> throwError err401
    Just (Entity uid user) ->
      if validatePassword (userHashedPassword user) (TE.encodeUtf8 loginFormPassword)
        then do
          let cookie = AuthCookie {authCookieUserUUID = userIdentifier user}
          TicklerServerEnv {..} <- ask
          mCookie <- liftIO $ makeSessionCookieBS envCookieSettings envJWTSettings cookie
          case mCookie of
            Nothing -> throwError err401
            Just setCookie -> do
              now <- liftIO getCurrentTime
              runDb $ update uid [UserLastLogin =. Just now]
              return $ addHeader (TE.decodeUtf8 setCookie) NoContent
        else throwError err401
