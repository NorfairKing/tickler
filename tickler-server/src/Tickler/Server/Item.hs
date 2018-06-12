{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
    ( makeTicklerItem
    , makeTriggeredItem
    , makeTicklerSynced
    , makeTicklerItemInfo
    , makeTriggeredItemInfo
    ) where

import Import

import Data.Time

import Tickler.API
import Tickler.Data

makeTicklerItem ::
       AccountUUID
    -> ItemUUID
    -> UTCTime
    -> UTCTime
    -> TypedTickle
    -> TicklerItem
makeTicklerItem u i cr sy Tickle {..} =
    let TypedItem {..} = tickleContent
    in TicklerItem
       { ticklerItemIdentifier = i
       , ticklerItemUserId = u
       , ticklerItemType = itemType
       , ticklerItemContents = itemData
       , ticklerItemCreated = cr
       , ticklerItemSynced = sy
       , ticklerItemScheduled = tickleScheduled
       }

makeTriggeredItem ::
       AccountUUID
    -> ItemUUID
    -> UTCTime
    -> UTCTime
    -> TypedTickle
    -> TicklerItem
makeTriggeredItem u i cr sy Tickle {..} =
    let TypedItem {..} = tickleContent
    in TicklerItem
       { ticklerItemIdentifier = i
       , ticklerItemUserId = u
       , ticklerItemType = itemType
       , ticklerItemContents = itemData
       , ticklerItemCreated = cr
       , ticklerItemSynced = sy
       , ticklerItemScheduled = tickleScheduled
       }

makeTicklerSynced :: TicklerItem -> Synced ItemUUID TypedTickle
makeTicklerSynced TicklerItem {..} =
    Synced
    { syncedUuid = ticklerItemIdentifier
    , syncedValue =
          Tickle
          { tickleContent =
                TypedItem
                {itemType = ticklerItemType, itemData = ticklerItemContents}
          , tickleScheduled = ticklerItemScheduled
          }
    , syncedCreated = ticklerItemCreated
    , syncedSynced = ticklerItemSynced
    }

makeTicklerItemInfo :: TicklerItem -> TypedItemInfo
makeTicklerItemInfo TicklerItem {..} =
    ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier
    , itemInfoContents =
          Tickle
          { tickleContent =
                TypedItem
                {itemType = ticklerItemType, itemData = ticklerItemContents}
          , tickleScheduled = ticklerItemScheduled
          }
    , itemInfoCreated = ticklerItemCreated
    , itemInfoSynced = ticklerItemSynced
    , itemInfoTriggered = Nothing
    }

makeTriggeredItemInfo :: TriggeredItem -> [TriggeredIntrayItem] -> TypedItemInfo
makeTriggeredItemInfo TriggeredItem {..} tiis =
    ItemInfo
    { itemInfoIdentifier = triggeredItemIdentifier
    , itemInfoContents =
          Tickle
          { tickleContent =
                TypedItem
                {itemType = triggeredItemType, itemData = triggeredItemContents}
          , tickleScheduled = triggeredItemScheduled
          }
    , itemInfoCreated = triggeredItemCreated
    , itemInfoSynced = triggeredItemSynced
    , itemInfoTriggered =
          Just
              TriggeredInfo
              { triggeredInfoTriggered = triggeredItemTriggered
              , triggeredInfoTriggerTriggerAttempts =
                    mapMaybe
                        (\TriggeredIntrayItem {..} ->
                             IntrayTriggerAttempt triggeredIntrayItemTrigger <$>
                             case ( triggeredIntrayItemIntrayItemUUID
                                  , triggeredIntrayItemError) of
                                 (Nothing, Nothing) -> Nothing
                                 (Just i, Nothing) ->
                                     Just $ IntrayAdditionSuccess i
                                 (Nothing, Just e) ->
                                     Just $ IntrayAdditionFailure e
                                 (Just _ ,Just e) ->
                                     Just $ IntrayAdditionFailure e) $
                    filter
                        ((== triggeredItemIdentifier) . triggeredIntrayItemItem)
                        tiis
              }
    }
