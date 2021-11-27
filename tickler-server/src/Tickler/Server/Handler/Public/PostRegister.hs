{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Public.PostRegister
  ( servePostRegister,
  )
where

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Database.Persist
import Import
import Servant hiding (BadPassword, NoSuchUser)
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostRegister :: Registration -> TicklerHandler NoContent
servePostRegister Registration {..} = do
  maybeHashedPassword <- liftIO $ passwordHash registrationPassword
  case maybeHashedPassword of
    Nothing -> throwError err400 {errBody = "Failed to hash password."}
    Just hashedPassword -> do
      uuid <- liftIO nextRandomUUID
      now <- liftIO getCurrentTime
      let user =
            User
              { userIdentifier = uuid,
                userUsername = registrationUsername,
                userHashedPassword = hashedPassword,
                userCreated = now,
                userLastLogin = Nothing
              }
      maybeUserEntity <- runDb . getBy $ UniqueUsername $ userUsername user
      case maybeUserEntity of
        Nothing -> runDb $ insert_ user
        Just _ ->
          throwError
            err409
              { errBody =
                  LB.fromStrict $
                    TE.encodeUtf8 $
                      T.unwords
                        [ "Account with the username",
                          usernameText registrationUsername,
                          "already exists."
                        ]
              }
  pure NoContent
