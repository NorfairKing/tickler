{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
  ( makeTicklerItem,
    makeTriggeredItem,
    makeTicklerAdded,
    makeTicklerItemInfo,
    makeTriggeredItemInfo,
  )
where

import Data.Mergeful.Timed as Mergeful
import Data.Time
import Import
import Tickler.API

makeTicklerItem ::
  AccountUUID -> ItemUUID -> UTCTime -> Mergeful.ServerTime -> TypedTickle -> TicklerItem
makeTicklerItem u i cr st Tickle {..} =
  let TypedItem {..} = tickleContent
   in TicklerItem
        { ticklerItemIdentifier = i,
          ticklerItemUserId = u,
          ticklerItemType = itemType,
          ticklerItemContents = itemData,
          ticklerItemCreated = cr,
          ticklerItemServerTime = st,
          ticklerItemScheduledDay = tickleScheduledDay,
          ticklerItemScheduledTime = tickleScheduledTime,
          ticklerItemRecurrence = tickleRecurrence
        }

makeTriggeredItem ::
  AccountUUID -> ItemUUID -> UTCTime -> Mergeful.ServerTime -> TypedTickle -> TicklerItem
makeTriggeredItem u i cr st Tickle {..} =
  let TypedItem {..} = tickleContent
   in TicklerItem
        { ticklerItemIdentifier = i,
          ticklerItemUserId = u,
          ticklerItemType = itemType,
          ticklerItemContents = itemData,
          ticklerItemCreated = cr,
          ticklerItemServerTime = st,
          ticklerItemScheduledDay = tickleScheduledDay,
          ticklerItemScheduledTime = tickleScheduledTime,
          ticklerItemRecurrence = tickleRecurrence
        }

makeTicklerAdded :: TicklerItem -> (ItemUUID, Mergeful.Timed (AddedItem TypedTickle))
makeTicklerAdded TicklerItem {..} =
  ( ticklerItemIdentifier,
    Timed
      { timedValue =
          AddedItem
            { addedItemContents =
                Tickle
                  { tickleContent =
                      TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents},
                    tickleScheduledDay = ticklerItemScheduledDay,
                    tickleScheduledTime = ticklerItemScheduledTime,
                    tickleRecurrence = ticklerItemRecurrence
                  },
              addedItemCreated = ticklerItemCreated
            },
        timedTime = ticklerItemServerTime
      }
  )

makeTicklerItemInfo :: TicklerItem -> TypedItemInfo
makeTicklerItemInfo TicklerItem {..} =
  ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier,
      itemInfoContents =
        Tickle
          { tickleContent = TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents},
            tickleScheduledDay = ticklerItemScheduledDay,
            tickleScheduledTime = ticklerItemScheduledTime,
            tickleRecurrence = ticklerItemRecurrence
          },
      itemInfoCreated = ticklerItemCreated,
      itemInfoTriggered = Nothing
    }

makeTriggeredItemInfo :: TriggeredItem -> [TriggeredIntrayItem] -> [TriggeredEmail] -> TypedItemInfo
makeTriggeredItemInfo TriggeredItem {..} tiis tes =
  ItemInfo
    { itemInfoIdentifier = triggeredItemIdentifier,
      itemInfoContents =
        Tickle
          { tickleContent =
              TypedItem {itemType = triggeredItemType, itemData = triggeredItemContents},
            tickleRecurrence = triggeredItemRecurrence,
            tickleScheduledDay = triggeredItemScheduledDay,
            tickleScheduledTime = triggeredItemScheduledTime
          },
      itemInfoCreated = triggeredItemCreated,
      itemInfoTriggered =
        Just
          TriggeredInfo
            { triggeredInfoTriggered = triggeredItemTriggered,
              triggeredInfoTriggerTriggerAttempts =
                concat
                  [ mapMaybe
                      ( \TriggeredIntrayItem {..} ->
                          IntrayTriggerAttempt triggeredIntrayItemTrigger
                            <$> case (triggeredIntrayItemIntrayItemUUID, triggeredIntrayItemError) of
                              (Nothing, Nothing) -> Nothing
                              (Just i, Nothing) -> Just $ IntrayAdditionSuccess i
                              (_, Just e) -> Just $ IntrayAdditionFailure e
                      )
                      $ filter ((== triggeredItemIdentifier) . triggeredIntrayItemItem) tiis,
                    mapMaybe
                      ( \TriggeredEmail {..} ->
                          EmailTriggerAttempt triggeredEmailTrigger
                            <$> case (triggeredEmailEmail, triggeredEmailError) of
                              (Nothing, Nothing) -> Nothing
                              (Just _, Nothing) -> Just EmailResultSent
                              (_, Just e) -> Just (EmailResultError e)
                      )
                      $ filter ((== triggeredItemIdentifier) . triggeredEmailItem) tes
                  ]
            }
    }
