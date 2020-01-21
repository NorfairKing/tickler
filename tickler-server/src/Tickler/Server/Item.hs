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

makeTicklerItem :: AccountUUID -> ItemUUID -> UTCTime -> UTCTime -> TypedTickle -> TicklerItem
makeTicklerItem u i cr sy Tickle {..} =
  let TypedItem {..} = tickleContent
   in TicklerItem
        { ticklerItemIdentifier = i
        , ticklerItemUserId = u
        , ticklerItemType = itemType
        , ticklerItemContents = itemData
        , ticklerItemCreated = cr
        , ticklerItemSynced = sy
        , ticklerItemScheduledDay = tickleScheduledDay
        , ticklerItemScheduledTime = tickleScheduledTime
        , ticklerItemRecurrence = tickleRecurrence
        }

makeTriggeredItem :: AccountUUID -> ItemUUID -> UTCTime -> UTCTime -> TypedTickle -> TicklerItem
makeTriggeredItem u i cr sy Tickle {..} =
  let TypedItem {..} = tickleContent
   in TicklerItem
        { ticklerItemIdentifier = i
        , ticklerItemUserId = u
        , ticklerItemType = itemType
        , ticklerItemContents = itemData
        , ticklerItemCreated = cr
        , ticklerItemSynced = sy
        , ticklerItemScheduledDay = tickleScheduledDay
        , ticklerItemScheduledTime = tickleScheduledTime
        , ticklerItemRecurrence = tickleRecurrence
        }

makeTicklerSynced :: TicklerItem -> (ItemUUID, Synced TypedTickle)
makeTicklerSynced TicklerItem {..} =
  ( ticklerItemIdentifier
  , Synced
      { syncedValue =
          Tickle
            { tickleContent = TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents}
            , tickleScheduledDay = ticklerItemScheduledDay
            , tickleScheduledTime = ticklerItemScheduledTime
            , tickleRecurrence = ticklerItemRecurrence
            }
      , syncedCreated = ticklerItemCreated
      , syncedSynced = ticklerItemSynced
      })

makeTicklerItemInfo :: TicklerItem -> TypedItemInfo
makeTicklerItemInfo TicklerItem {..} =
  ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier
    , itemInfoContents =
        Tickle
          { tickleContent = TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents}
          , tickleScheduledDay = ticklerItemScheduledDay
          , tickleScheduledTime = ticklerItemScheduledTime
          , tickleRecurrence = ticklerItemRecurrence
          }
    , itemInfoCreated = ticklerItemCreated
    , itemInfoSynced = ticklerItemSynced
    , itemInfoTriggered = Nothing
    }

makeTriggeredItemInfo :: TriggeredItem -> [TriggeredIntrayItem] -> [TriggeredEmail] -> TypedItemInfo
makeTriggeredItemInfo TriggeredItem {..} tiis tes =
  ItemInfo
    { itemInfoIdentifier = triggeredItemIdentifier
    , itemInfoContents =
        Tickle
          { tickleContent =
              TypedItem {itemType = triggeredItemType, itemData = triggeredItemContents}
          , tickleRecurrence = triggeredItemRecurrence
          , tickleScheduledDay = triggeredItemScheduledDay
          , tickleScheduledTime = triggeredItemScheduledTime
          }
    , itemInfoCreated = triggeredItemCreated
    , itemInfoSynced = triggeredItemSynced
    , itemInfoTriggered =
        Just
          TriggeredInfo
            { triggeredInfoTriggered = triggeredItemTriggered
            , triggeredInfoTriggerTriggerAttempts =
                concat
                  [ mapMaybe
                      (\TriggeredIntrayItem {..} ->
                         IntrayTriggerAttempt triggeredIntrayItemTrigger <$>
                         case (triggeredIntrayItemIntrayItemUUID, triggeredIntrayItemError) of
                           (Nothing, Nothing) -> Nothing
                           (Just i, Nothing) -> Just $ IntrayAdditionSuccess i
                           (_, Just e) -> Just $ IntrayAdditionFailure e) $
                    filter ((== triggeredItemIdentifier) . triggeredIntrayItemItem) tiis
                  , mapMaybe
                      (\TriggeredEmail {..} ->
                         EmailTriggerAttempt triggeredEmailTrigger <$>
                         case (triggeredEmailEmail, triggeredEmailError) of
                           (Nothing, Nothing) -> Nothing
                           (Just _, Nothing) -> Just EmailResultSent
                           (_, Just e) -> Just (EmailResultError e)) $
                    filter ((== triggeredItemIdentifier) . triggeredEmailItem) tes
                  ]
            }
    }
