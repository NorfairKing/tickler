{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Triggers
  ( getTriggersR,
    postTriggerAddIntrayR,
    postTriggerAddEmailR,
    postTriggerDeleteR,
  )
where

import qualified Data.Text as T
import Import
import qualified Intray.Data as Intray
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
    mun <-
      do
        errOrAi <- runClient $ clientGetAccountInfo t
        case errOrAi of
          Left _ -> pure Nothing
          Right AccountInfo {..} -> pure $ Just accountInfoUsername
    token <- genToken
    pure $(widgetFile "add-intray-trigger")

makeAddEmailTriggerWidget :: Handler Widget
makeAddEmailTriggerWidget = do
  token <- genToken
  pure $(widgetFile "add-email-trigger")

addIntrayTriggerForm :: FormInput Handler AddIntrayTrigger
addIntrayTriggerForm =
  AddIntrayTrigger
    <$> ireq
      ( checkMMap
          ( pure
              . ( \case
                    Nothing -> Left ("Invalid URL" :: Text)
                    Just u -> pure u
                )
              . parseBaseUrl
              . T.unpack
          )
          (T.pack . showBaseUrl)
          textField
      )
      "url"
    <*> ireq
      ( checkMMap
          ( pure
              . ( \t ->
                    case Intray.parseUsername t of
                      Nothing -> Left ("Invalid Intray Username" :: Text)
                      Just un -> pure un
                )
          )
          Intray.usernameText
          textField
      )
      "username"
    <*> ireq
      ( checkMMap
          ( pure
              . ( \t ->
                    case Intray.parseAccessKeySecretText t of
                      Left _ -> Left ("Invalid access key" :: Text)
                      Right aks -> pure aks
                )
          )
          Intray.accessKeySecretText
          textField
      )
      "access-key"

postTriggerAddIntrayR :: Handler Html
postTriggerAddIntrayR =
  withLogin $ \t -> do
    ait <- runInputPost addIntrayTriggerForm
    errOrRes <- runClientOrErr $ clientPostAddIntrayTrigger t ait
    case errOrRes of
      Left err -> do
        addMessage
          "error"
          [shamlet|
            <div .ui .segment>
                Failed to login to the intray instance:
                <pre style="overflow:auto;">
                    #{err}
                    |]
        redirect TriggersR
      Right _ -> do
        addMessage "success" "New intray trigger added."
        redirect TriggersR

addEmailTriggerForm :: FormInput Handler AddEmailTrigger
addEmailTriggerForm =
  AddEmailTrigger
    <$> ireq (checkMMap (pure . left T.pack . emailValidateFromText) emailAddressText textField) "email"

postTriggerAddEmailR :: Handler Html
postTriggerAddEmailR =
  withLogin $ \t -> do
    aet <- runInputPost addEmailTriggerForm
    void $ runClientOrErr $ clientPostAddEmailTrigger t aet
    redirect TriggersR

postTriggerDeleteR :: TriggerUUID -> Handler Html
postTriggerDeleteR uuid =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteTrigger t uuid
    redirect TriggersR
