{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetItems where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API
import Tickler.Data

import Tickler.Server.Item
import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetItems :: AuthResult AuthCookie -> Maybe ItemFilter -> TicklerHandler [TypedItemInfo]
serveGetItems (Authenticated AuthCookie {..}) mif = do
  let getTicklerItems = do
        itemsEnts <-
          runDb $ selectList [TicklerItemUserId ==. authCookieUserUUID] [Asc TicklerItemCreated]
        pure $ map (makeTicklerItemInfo . entityVal) itemsEnts
  let getTriggeredItems = do
        itemsEnts <-
          runDb $ selectList [TriggeredItemUserId ==. authCookieUserUUID] [Asc TriggeredItemCreated]
        triggeredItemEns <-
          runDb $
          selectList
            [TriggeredIntrayItemItem <-. map (triggeredItemIdentifier . entityVal) itemsEnts]
            []
        triggeredEmailEns <-
          runDb $
          selectList [TriggeredEmailItem <-. map (triggeredItemIdentifier . entityVal) itemsEnts] []
        pure $
          map
            (\ie ->
               makeTriggeredItemInfo
                 (entityVal ie)
                 (map entityVal triggeredItemEns)
                 (map entityVal triggeredEmailEns))
            itemsEnts
  case mif of
    Just OnlyUntriggered -> getTicklerItems
    Just OnlyTriggered -> getTriggeredItems
    Nothing -> liftA2 (++) getTicklerItems getTriggeredItems
serveGetItems _ _ = throwAll err401
