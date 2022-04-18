{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Add where

import Import
import qualified Network.HTTP.Types as Http
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Item
import Yesod

getAddR :: Handler Html
getAddR =
  withLogin $ \t -> do
    AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
    let canAdd =
          case accountInfoStatus of
            HasPaid _ -> True
            NoPaymentNecessary -> True
            HasNotPaid itemsLeft -> itemsLeft >= 1
    w <- makeEditItemFormWidget Nothing
    withNavBar $(widgetFile "add")

postAddR :: Handler Html
postAddR =
  withLogin $ \t -> do
    AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
    tickle <- handleEditItemForm
    errOrRes <- runClient $ clientPostItem t tickle
    case errOrRes of
      Left err ->
        handleStandardServantErrs err $ \resp ->
          case responseStatusCode resp of
            c
              | c == Http.paymentRequired402 -> do
                sendResponseStatus
                  Http.status402
                  "You have reached the limit of the free plan, subscribe to be able to add more items. Click 'Account' to get started."
              | otherwise ->
                sendResponseStatus Http.status500 $ show resp
      Right uuid -> redirect $ EditR uuid
