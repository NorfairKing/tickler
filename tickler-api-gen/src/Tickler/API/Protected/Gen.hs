{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Protected.Gen where

import qualified Data.Text.Encoding as TE
import Import
import Intray.Data.Gen ()
import Tickler.API.Protected.Types
import Tickler.Data
import Tickler.Data.Gen ()

instance GenValid ItemFilter where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TypedItem where
  genValid = do
    ti <- genValid
    case ti of
      TextItem -> do
        t <- genValid
        pure TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}
  shrinkValid = shrinkValidStructurally

instance GenValid a => GenValid (Tickle a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TriggerAttempt where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EmailTriggerResult where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid IntrayTriggerResult where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TriggeredInfo where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid a => GenValid (AddedItem a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid a => GenValid (ItemInfo a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid a => GenValid (TriggerInfo a) where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid TypedTriggerInfo where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid IntrayTriggerInfo where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid EmailTriggerInfo where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddIntrayTrigger where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid AddEmailTrigger where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncRequest where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SyncResponse where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
