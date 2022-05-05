{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TriggerAddEmail (postTriggerAddEmailR) where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

addEmailTriggerForm :: FormInput Handler AddEmailTrigger
addEmailTriggerForm =
  AddEmailTrigger
    <$> ireq
      ( checkMMap
          (pure . (Right :: EmailAddress -> Either Text EmailAddress) . emailAddress)
          emailAddressText
          textField
      )
      "email-address"

postTriggerAddEmailR :: Handler Html
postTriggerAddEmailR =
  withLogin $ \t -> do
    aet <- runInputPost addEmailTriggerForm
    void $ runClientOrErr $ clientPostEmailTrigger t aet
    redirect TriggersR
