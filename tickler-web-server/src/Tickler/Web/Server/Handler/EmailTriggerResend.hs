{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.EmailTriggerResend (postTriggerEmailResendR) where

import Import
import qualified Network.HTTP.Types as HTTP
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

postTriggerEmailResendR :: TriggerUUID -> Handler Html
postTriggerEmailResendR tuuid =
  withLogin $ \t -> do
    errOrNoContent <- runClient $ clientPostEmailTriggerResendVerificationEmail t tuuid
    case errOrNoContent of
      Left err -> handleStandardServantErrs err $ \resp ->
        case HTTP.statusCode (responseStatusCode resp) of
          409 -> do
            addPositiveMessage "Email trigger already verified."
            redirect TriggersR
          400 -> do
            addNegativeMessage "Verification email already scheduled."
            redirect TriggersR
          c ->
            sendResponseStatus HTTP.status500
              $ unwords
                [ "Error while connecting to the API:",
                  show c
                ]
      Right NoContent -> do
        addPositiveMessage "Verification email resent"
        redirect TriggersR
