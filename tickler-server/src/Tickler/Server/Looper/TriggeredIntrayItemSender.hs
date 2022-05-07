{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredIntrayItemSender
  ( runTriggeredIntrayItemSender,
  )
where

import Conduit
import Control.Monad.Logger
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite
import Import
import qualified Intray.Client as Intray
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant
import Servant.Auth.Client
import Servant.Client
import Tickler.Client
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types
import Web.Cookie

runTriggeredIntrayItemSender :: Looper ()
runTriggeredIntrayItemSender = do
  acqTriggeredIntrayItemSource <-
    runDb $
      selectSourceRes
        [TriggeredIntrayItemIntrayItemUUID ==. Nothing, TriggeredIntrayItemError ==. Nothing]
        []
  withAcquire acqTriggeredIntrayItemSource $ \triggeredIntrayItemSource ->
    runConduit $ triggeredIntrayItemSource .| C.mapM_ sendTriggeredIntrayItem

sendTriggeredIntrayItem :: Entity TriggeredIntrayItem -> Looper ()
sendTriggeredIntrayItem (Entity tii TriggeredIntrayItem {..}) = do
  mtrig <- runDb $ getBy $ UniqueIntrayTrigger triggeredIntrayItemTrigger
  case mtrig of
    Nothing -> logErrorN "Trigger does not exist anymore."
    Just (Entity _ IntrayTrigger {..}) -> do
      man <- liftIO $ Http.newManager Http.tlsManagerSettings
      mi <- runDb $ getBy $ UniqueTriggeredItemIdentifier triggeredIntrayItemItem
      case mi of
        Nothing -> logErrorN "Triggered item does not exist anymore."
        Just (Entity _ TriggeredItem {..}) -> do
          let env = ClientEnv man intrayTriggerUrl Nothing
          errOrUuid <-
            liftIO $
              flip runClientM env $
                do
                  let loginForm =
                        Intray.LoginForm
                          { Intray.loginFormUsername = intrayTriggerUsername,
                            Intray.loginFormPassword =
                              Intray.accessKeySecretText intrayTriggerAccessKey
                          }
                  res <- Intray.clientPostLogin loginForm
                  case res of
                    Headers Intray.NoContent (HCons sessionHeader HNil) ->
                      case sessionHeader of
                        MissingHeader -> pure $ Left "Login should return a session header"
                        UndecodableHeader _ ->
                          pure $ Left "Login should return a decodable session header"
                        Header setCookieText -> do
                          let cookies = parseSetCookie . TE.encodeUtf8 <$> T.lines setCookieText
                              jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
                          case jwtCookie of
                            Nothing ->
                              pure $
                                Left "No JWT-Cookie was found in the Set-Cookie session header."
                            Just session -> do
                              let token = Token $ setCookieValue session
                              let item =
                                    Intray.TypedItem
                                      { Intray.itemType = Intray.TextItem,
                                        Intray.itemData = TE.encodeUtf8 triggeredItemContents
                                      }
                              Right <$> Intray.clientPostAddItem token item
          case errOrUuid of
            Left (ConnectionError err) -> do
              logErrorN $
                T.pack $
                  unwords ["Failed to add item to intray:", show err]
              runDb $ update tii [TriggeredIntrayItemError =. Just (T.pack (show err))]
            Left err -> do
              logErrorN $
                T.pack $
                  unwords ["Failed to add item to intray:", show err]
              runDb $ update tii [TriggeredIntrayItemError =. Just (T.pack (show err))]
            Right (Left err) -> do
              logErrorN $
                T.pack $
                  unwords ["The intray server did something wrong:", err]
              runDb $ update tii [TriggeredIntrayItemError =. Just (T.pack err)]
            Right (Right uuid) ->
              runDb $ update tii [TriggeredIntrayItemIntrayItemUUID =. Just uuid]
