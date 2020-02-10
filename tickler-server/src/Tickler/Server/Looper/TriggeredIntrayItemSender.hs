{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredIntrayItemSender
  ( runTriggeredIntrayItemSender
  ) where

import Import

import qualified Data.Text as T

import Control.Monad.Logger
import Database.Persist.Sqlite
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Web.Cookie

import Servant
import Servant.Auth.Client
import Servant.Client

import qualified Intray.Client as Intray

import Tickler.Client

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runTriggeredIntrayItemSender :: () -> Looper ()
runTriggeredIntrayItemSender _ = do
  logInfoNS "TriggeredIntraySender" "Starting sending TriggeredIntrayItems."
  tiis <-
    runDb $
    selectList
      [TriggeredIntrayItemIntrayItemUUID ==. Nothing, TriggeredIntrayItemError ==. Nothing]
      []
  unless (null tiis) $
    forM_ tiis $ \(Entity tii TriggeredIntrayItem {..}) -> do
      mtrig <- runDb $ getBy $ UniqueIntrayTrigger triggeredIntrayItemTrigger
      case mtrig of
        Nothing -> logErrorNS "TriggeredIntraySender" "Trigger does not exist anymore."
        Just (Entity _ IntrayTrigger {..}) -> do
          man <- liftIO $ Http.newManager Http.tlsManagerSettings
          mi <- runDb $ getBy $ UniqueTriggeredItemIdentifier triggeredIntrayItemItem
          case mi of
            Nothing -> logErrorNS "TriggeredIntraySender" "Triggered item does not exist anymore."
            Just (Entity _ TriggeredItem {..}) -> do
              let env = ClientEnv man intrayTriggerUrl Nothing
              errOrUuid <-
                liftIO $
                flip runClientM env $ do
                  let loginForm =
                        Intray.LoginForm
                          { Intray.loginFormUsername = intrayTriggerUsername
                          , Intray.loginFormPassword =
                              Intray.accessKeySecretText intrayTriggerAccessKey
                          }
                  res <- Intray.clientPostLogin loginForm
                  case res of
                    Headers Intray.NoContent (HCons _ (HCons sessionHeader HNil)) ->
                      case sessionHeader of
                        MissingHeader -> pure $ Left "Login should return a session header"
                        UndecodableHeader _ ->
                          pure $ Left "Login should return a decodable session header"
                        Header session -> do
                          let token = Token $ setCookieValue session
                          let item =
                                Intray.TypedItem
                                  { Intray.itemType =
                                      case triggeredItemType of
                                        TextItem -> Intray.TextItem
                                  , Intray.itemData = triggeredItemContents
                                  }
                          Right <$> Intray.clientPostAddItem token item
              case errOrUuid of
                Left (ConnectionError err) -> do
                  logErrorNS "TriggeredIntraySender" $
                    T.unwords ["Failed to add item to intray:", T.pack (show err)]
                  runDb $ update tii [TriggeredIntrayItemError =. Just (T.pack (show err))]
                Left err -> do
                  logErrorNS "TriggeredIntraySender" $
                    T.unwords ["Failed to add item to intray:", T.pack (show err)]
                  runDb $ update tii [TriggeredIntrayItemError =. Just (T.pack (show err))]
                Right (Left err) -> do
                  logErrorNS "TriggeredIntraySender" $
                    T.unwords ["The intray server did something wrong:", T.pack err]
                  runDb $ update tii [TriggeredIntrayItemError =. Just (T.pack err)]
                Right (Right uuid) ->
                  runDb $ update tii [TriggeredIntrayItemIntrayItemUUID =. Just uuid]
  logInfoNS "TriggeredIntraySender" "Finished sending TriggeredIntrayItems."
