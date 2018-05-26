{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredIntrayItemSender
    ( runTriggeredIntrayItemSender
    ) where

import Import

import Control.Concurrent

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool
import Database.Persist.Sqlite
import qualified Network.HTTP.Client as Http
import Web.Cookie

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text

import Servant
import Servant.Auth.Client
import Servant.Auth.Server as Auth
import Servant.Client

import qualified Intray.Client as Intray

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runTriggeredIntrayItemSender :: () -> Looper ()
runTriggeredIntrayItemSender _ = do
    logInfoNS "TriggeredIntraySender" "Starting sending TriggeredIntrayItems."
    tiis <-
        runDb $ selectList [TriggeredIntrayItemIntrayItemUUID ==. Nothing] []
    unless (null tiis) $ do
        man <- liftIO $ Http.newManager Http.defaultManagerSettings
        forM_ tiis $ \(Entity tii TriggeredIntrayItem {..}) -> do
            mtrig <-
                runDb $ getBy $ UniqueIntrayTrigger $ triggeredIntrayItemTrigger
            case mtrig of
                Nothing ->
                    logErrorNS
                        "TriggeredIntraySender"
                        "Trigger does not exist anymore."
                Just (Entity _ IntrayTrigger {..}) -> do
                    mi <-
                        runDb $
                        getBy $
                        UniqueTriggeredItemIdentifier triggeredIntrayItemItem
                    case mi of
                        Nothing ->
                            logErrorNS
                                "TriggeredIntraySender"
                                "Triggered item does not exist anymore."
                        Just (Entity _ TriggeredItem {..}) -> do
                            let env = ClientEnv man intrayTriggerUrl Nothing
                            case Intray.parseUsername intrayTriggerUsername of
                                Nothing ->
                                    logErrorNS
                                        "TriggeredIntraySender"
                                        "Trigger's username is invalid."
                                Just un -> do
                                    errOrUuid <-
                                        liftIO $
                                        flip runClientM env $ do
                                            let loginForm =
                                                    Intray.LoginForm
                                                    { Intray.loginFormUsername =
                                                          un
                                                    , Intray.loginFormPassword =
                                                          intrayTriggerAccessKey
                                                    }
                                            Headers Intray.NoContent (HCons _ (HCons sessionHeader HNil)) <-
                                                Intray.clientPostLogin loginForm
                                            case sessionHeader of
                                                MissingHeader -> do
                                                    pure $
                                                        Left
                                                            "Login should return a session header"
                                                UndecodableHeader _ ->
                                                    pure $
                                                    Left
                                                        "Login should return a decodable session header"
                                                Header session -> do
                                                    let token =
                                                            Token $
                                                            setCookieValue
                                                                session
                                                    let item =
                                                            Intray.TypedItem
                                                            { Intray.itemType =
                                                                  case triggeredItemType of
                                                                      TextItem ->
                                                                          Intray.TextItem
                                                            , Intray.itemData =
                                                                  triggeredItemContents
                                                            }
                                                    Right <$>
                                                        Intray.clientPostAddItem
                                                            token
                                                            item
                                    case errOrUuid of
                                        Left err ->
                                            logErrorNS "TriggeredIntraySender" $
                                            T.unwords
                                                [ "Failed to add item to intray:"
                                                , T.pack (show err)
                                                ]
                                        Right (Left err) ->
                                            logErrorNS "TriggeredIntraySender" $
                                            T.unwords
                                                [ "The intray server did something wrong:"
                                                , T.pack (show err)
                                                ]
                                        Right (Right uuid) ->
                                            runDb $
                                            update
                                                tii
                                                [ TriggeredIntrayItemIntrayItemUUID =.
                                                  Just (uuidText uuid)
                                                ]
    logInfoNS "TriggeredIntraySender" "Finished sending TriggeredIntrayItems."
