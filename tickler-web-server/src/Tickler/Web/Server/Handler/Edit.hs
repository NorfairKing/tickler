{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Handler.Edit
  ( getEditR,
    postEditR,
  )
where

import Import
import qualified Network.HTTP.Types as Http
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Item
import Yesod

getEditR :: ItemUUID -> Handler Html
getEditR uuid =
  withLogin $ \t -> do
    errOrRes <- runClient $ clientGetItem t uuid
    case errOrRes of
      Left err ->
        handleStandardServantErrs err $ \resp ->
          case responseStatusCode resp of
            c
              | c == Http.notFound404 -> notFound
              | otherwise -> sendResponseStatus Http.status500 $ show resp
      Right ii -> do
        w <- makeEditItemFormWidget (Just (uuid, ii))
        withNavBar w

postEditR :: ItemUUID -> Handler Html
postEditR uuid =
  withLogin $ \t -> do
    AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
    tickle <- handleEditItemForm
    errOrRes <- runClient $ clientPutItem t uuid tickle
    case errOrRes of
      Left err ->
        handleStandardServantErrs err $ \resp ->
          case responseStatusCode resp of
            c
              | c == Http.notFound404 -> notFound
              | otherwise -> sendResponseStatus Http.status500 $ show resp
      Right NoContent -> redirect $ EditR uuid
