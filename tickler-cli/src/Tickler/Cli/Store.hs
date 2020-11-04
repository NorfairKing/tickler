module Tickler.Cli.Store
  ( Store (..),
    readStore,
    readStoreOrEmpty,
    readStoreSize,
    anyUnsynced,
    writeStore,
  )
where

import qualified Data.Map as M
import Data.Mergeful (clientStoreAddedItems, clientStoreSize)
import Import
import Tickler.Cli.JSON
import Tickler.Cli.OptParse
import Tickler.Cli.Path
import Tickler.Client

{-# ANN module "HLint: ignore Use &&" #-}

readStore :: CliM (Maybe Store)
readStore = storePath >>= readJSON

readStoreOrEmpty :: CliM Store
readStoreOrEmpty = fromMaybe emptyStore <$> readStore

readStoreSize :: CliM Word
readStoreSize = clientStoreSize . storeTickles <$> readStoreOrEmpty

anyUnsynced :: Store -> Bool
anyUnsynced = not . M.null . clientStoreAddedItems . storeTickles

writeStore :: Store -> CliM ()
writeStore s = storePath >>= (`writeJSON` s)
