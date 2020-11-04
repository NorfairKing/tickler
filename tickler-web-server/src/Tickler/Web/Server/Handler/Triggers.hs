{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Triggers
  ( getTriggersR,
    postAddIntrayTriggerR,
    postAddEmailTriggerR,
    postDeleteTriggerR,
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

makeTriggerInfoWidget :: TriggerInfo TypedTriggerInfo -> Handler Widget
makeTriggerInfoWidget tti =
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
  token <- genToken
  pure $(widgetFile "intray-trigger")

makeEmailTriggerWidget :: TriggerInfo EmailTriggerInfo -> Handler Widget
makeEmailTriggerWidget TriggerInfo {..} = do
  let EmailTriggerInfo {..} = triggerInfo
  token <- genToken
  pure $(widgetFile "email-trigger")

makeAddIntrayTriggerWidget :: Handler Widget
makeAddIntrayTriggerWidget =
  withLogin $ \t -> do
    LoopersInfo {..} <- runClientOrErr clientGetLoopersInfo
    if any
      ((== LooperStatusDisabled) . looperInfoStatus)
      [ triggeredIntrayItemSenderLooperInfo,
        triggeredIntrayItemSchedulerLooperInfo,
        triggererLooperInfo
      ]
      then pure mempty
      else do
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
  LoopersInfo {..} <- runClientOrErr clientGetLoopersInfo
  if any
    ((== LooperStatusDisabled) . looperInfoStatus)
    [ verificationEmailConverterLooperInfo,
      triggeredEmailSchedulerLooperInfo,
      triggeredEmailConverterLooperInfo,
      emailerLooperInfo
    ]
    then pure mempty
    else do
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
                      Nothing -> Left ("Invalid access key" :: Text)
                      Just aks -> pure aks
                )
          )
          Intray.accessKeySecretText
          textField
      )
      "access-key"

postAddIntrayTriggerR :: Handler Html
postAddIntrayTriggerR =
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

postAddEmailTriggerR :: Handler Html
postAddEmailTriggerR =
  withLogin $ \t -> do
    aet <- runInputPost addEmailTriggerForm
    void $ runClientOrErr $ clientPostAddEmailTrigger t aet
    redirect TriggersR

postDeleteTriggerR :: TriggerUUID -> Handler Html
postDeleteTriggerR uuid =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteTrigger t uuid
    redirect TriggersR
