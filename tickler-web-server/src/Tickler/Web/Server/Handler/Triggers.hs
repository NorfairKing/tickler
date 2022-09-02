{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Triggers (getTriggersR) where

import Import
import Servant.Client.Core
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

getTriggersR :: Handler Html
getTriggersR =
  withLogin $ \t -> do
    triggers <- runClientOrErr $ clientGetTriggers t
    triggerWidgets <- mapM makeTriggerInfoWidget triggers
    addIntrayTriggerWidget <- makeAddIntrayTriggerWidget
    addEmailTriggerWidget <- makeAddEmailTriggerWidget
    withNavBar $(widgetFile "triggers")

makeTriggerInfoWidget :: TriggerInfo -> Handler Widget
makeTriggerInfoWidget tti =
  case triggerInfo tti of
    TriggerIntray iti -> makeIntrayTriggerWidget (triggerInfoIdentifier tti) iti
    TriggerEmail eti -> makeEmailTriggerWidget (triggerInfoIdentifier tti) eti

makeIntrayTriggerWidget :: TriggerUUID -> IntrayTriggerInfo -> Handler Widget
makeIntrayTriggerWidget triggerInfoIdentifier IntrayTriggerInfo {..} = do
  token <- genToken
  pure $(widgetFile "intray-trigger")

makeEmailTriggerWidget :: TriggerUUID -> EmailTriggerInfo -> Handler Widget
makeEmailTriggerWidget triggerInfoIdentifier EmailTriggerInfo {..} = do
  token <- genToken
  pure $(widgetFile "email-trigger")

makeAddIntrayTriggerWidget :: Handler Widget
makeAddIntrayTriggerWidget =
  withLogin $ \t -> do
    defaultIntrayUrl <- getsYesod appDefaultIntrayUrl
    AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
    token <- genToken
    pure $(widgetFile "add-intray-trigger")

makeAddEmailTriggerWidget :: Handler Widget
makeAddEmailTriggerWidget = do
  token <- genToken
  pure $(widgetFile "add-email-trigger")
