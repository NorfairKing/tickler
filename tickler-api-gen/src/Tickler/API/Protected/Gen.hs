{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Protected.Gen where

import qualified Data.Text.Encoding as TE
import Import
import Intray.Data.Gen ()
import Tickler.API.Protected.Types
import Tickler.Data.Gen ()

instance GenValid ItemFilter

instance GenValid TypedItem where
  genValid = do
    ti <- genValid
    case ti of
      TextItem -> do
        t <- genValid
        pure TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}
  shrinkValid = shrinkValidStructurally

instance GenValid a => GenValid (Tickle a)

instance GenValid TriggerAttempt

instance GenValid EmailTriggerResult

instance GenValid IntrayTriggerResult

instance GenValid TriggeredInfo

instance GenValid a => GenValid (AddedItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid a => GenValid (ItemInfo a)

instance GenValid a => GenValid (TriggerInfo a)

instance GenValid TypedTriggerInfo

instance GenValid IntrayTriggerInfo

instance GenValid EmailTriggerInfo

instance GenValid AddIntrayTrigger

instance GenValid AddEmailTrigger
