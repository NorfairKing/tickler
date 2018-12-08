{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.EmailTriggerVerify
    ( getEmailTriggerVerifyR
    ) where

import Import

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation

getEmailTriggerVerifyR :: TriggerUUID -> EmailVerificationKey -> Handler Html
getEmailTriggerVerifyR tuuid evk =
    withLogin $ \t -> do
        NoContent <- runClientOrErr $ clientPostEmailTriggerVerify t tuuid evk
        redirect TriggersR
