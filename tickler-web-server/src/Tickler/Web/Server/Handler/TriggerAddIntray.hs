{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.TriggerAddIntray (postTriggerAddIntrayR) where

import qualified Data.Text as T
import Import
import qualified Intray.Data as Intray
import Servant.Client.Core
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

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
    errOrRes <- runClientOrErr $ clientPostIntrayTrigger t ait
    case errOrRes of
      Left err -> do
        addNegativeMessage
          [shamlet|
            <div .ui .segment>
                Failed to login to the intray instance:
                <pre style="overflow:auto;">
                    #{err}
                    |]
        redirect TriggersR
      Right _ -> do
        addPositiveMessage "New intray trigger added."
        redirect TriggersR
