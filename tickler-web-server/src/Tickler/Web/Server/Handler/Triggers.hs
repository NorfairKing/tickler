{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Triggers
    ( getTriggersR
    , postAddIntrayTriggerR
    , postAddEmailTriggerR
    ) where

import Import

import qualified Data.Text as T
import Data.Time

import Servant.Client.Core

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.OptParse.Types
import Tickler.Web.Server.Time

getTriggersR :: Handler Html
getTriggersR =
    withLogin $ \t -> do
        triggers <- runClientOrErr $ clientGetTriggers t
        triggerWidgets <- mapM makeTriggerInfoWidget triggers
        addIntrayTriggerWidget <- makeAddIntrayTriggerWidget
        addEmailTriggerWidget <- makeAddEmailTriggerWidget
        withNavBar $(widgetFile "triggers")

makeTriggerInfoWidget :: TriggerInfo TypedTriggerInfo -> Handler Widget
makeTriggerInfoWidget tti = do
    case typedTriggerInfoType $ triggerInfo tti of
        IntrayTriggerType ->
            case decodeTriggerInfo IntrayTriggerType tti of
                Nothing -> pure "Failed to decode intray trigger."
                Just iti -> makeIntrayTriggerWidget iti
        EmailTriggerType ->
            case decodeTriggerInfo EmailTriggerType tti of
                Nothing -> pure "Failed to decode email trigger."
                Just eti -> makeEmailTriggerWidget eti

makeIntrayTriggerWidget :: TriggerInfo IntrayTriggerInfo -> Handler Widget
makeIntrayTriggerWidget TriggerInfo {..} = do
    let IntrayTriggerInfo {..} = triggerInfo
    pure $(widgetFile "intray-trigger")

makeEmailTriggerWidget :: TriggerInfo EmailTriggerInfo -> Handler Widget
makeEmailTriggerWidget TriggerInfo {..} = do
    let EmailTriggerInfo {..} = triggerInfo
    pure $(widgetFile "email-trigger")

makeAddIntrayTriggerWidget :: Handler Widget
makeAddIntrayTriggerWidget = do
    defaultIntrayUrl <- getsYesod appDefaultIntrayUrl
    token <- genToken
    pure $(widgetFile "add-intray-trigger")

makeAddEmailTriggerWidget :: Handler Widget
makeAddEmailTriggerWidget = do
    token <- genToken
    pure $(widgetFile "add-email-trigger")

addIntrayTriggerForm :: FormInput Handler AddIntrayTrigger
addIntrayTriggerForm =
    AddIntrayTrigger <$>
    ireq
        (checkMMap
             (pure .
              (\mu ->
                   case mu of
                       Nothing -> Left ("Invalid URL" :: Text)
                       Just u -> pure u) .
              parseBaseUrl . T.unpack)
             (T.pack . showBaseUrl)
             textField)
        "url" <*>
    ireq textField "username" <*>
    ireq textField "access-key"

postAddIntrayTriggerR :: Handler Html
postAddIntrayTriggerR =
    withLogin $ \t -> do
        ait <- runInputPost addIntrayTriggerForm
        runClientOrErr $ clientPostAddIntrayTrigger t ait
        redirect TriggersR

addEmailTriggerForm :: FormInput Handler AddEmailTrigger
addEmailTriggerForm =
    AddEmailTrigger <$>
    ireq
        (checkMMap
             (pure . left T.pack . emailValidateFromText)
             emailAddressText
             textField)
        "email"

postAddEmailTriggerR :: Handler Html
postAddEmailTriggerR =
    withLogin $ \t -> do
        aet <- runInputPost addEmailTriggerForm
        runClientOrErr $ clientPostAddEmailTrigger t aet
        redirect TriggersR
