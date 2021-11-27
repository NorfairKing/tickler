{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.GetItems where

import Database.Esqueleto ((^.))
import qualified Database.Esqueleto as E
import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

serveGetItems :: AuthCookie -> Maybe ItemFilter -> TicklerHandler [TypedItemInfo]
serveGetItems AuthCookie {..} mif =
  runDb $ do
    let getTicklerItems = do
          itemsEnts <-
            selectList [TicklerItemUserId ==. authCookieUserUUID] [Asc TicklerItemCreated]
          pure $ map (makeTicklerItemInfo . entityVal) itemsEnts
    let getTriggeredItems = do
          itemsEnts <-
            selectList [TriggeredItemUserId ==. authCookieUserUUID] [Asc TriggeredItemCreated]
          triggeredItemEns <-
            E.select $
              E.from $
                \(triggeredItem `E.InnerJoin` triggeredIntrayItem) -> do
                  E.on
                    ( triggeredIntrayItem ^. TriggeredIntrayItemItem E.==. triggeredItem
                        ^. TriggeredItemIdentifier
                    )
                  E.where_ (triggeredItem ^. TriggeredItemUserId E.==. E.val authCookieUserUUID)
                  pure triggeredIntrayItem
          triggeredEmailEns <-
            E.select $
              E.from $
                \(triggeredItem `E.InnerJoin` triggeredEmailItem) -> do
                  E.on
                    ( triggeredEmailItem ^. TriggeredEmailItem E.==. triggeredItem
                        ^. TriggeredItemIdentifier
                    )
                  E.where_ (triggeredItem ^. TriggeredItemUserId E.==. E.val authCookieUserUUID)
                  pure triggeredEmailItem
          pure $
            map
              ( \ie ->
                  makeTriggeredItemInfo
                    (entityVal ie)
                    (map entityVal triggeredItemEns)
                    (map entityVal triggeredEmailEns)
              )
              itemsEnts
    case mif of
      Just OnlyUntriggered -> getTicklerItems
      Just OnlyTriggered -> getTriggeredItems
      Nothing -> liftA2 (++) getTicklerItems getTriggeredItems
