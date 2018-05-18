{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Commands.Show
    ( showItem
    ) where

import Import

import qualified Data.Text as T
import Data.Time
import Text.Time.Pretty

import Tickler.API

import Tickler.Cli.LastSeen
import Tickler.Cli.OptParse
import Tickler.Cli.Store
import Tickler.Cli.Sync

showItem :: CliM ()
showItem = do
    mls <- readLastSeen
    mli <-
        case mls of
            Nothing -> syncAndReturn lastItemInStore
            Just li -> pure $ Just li
    case mli of
        Nothing -> liftIO $ putStrLn "Done."
        Just li -> do
            writeLastSeen li
            now <- liftIO getCurrentTime
            liftIO $ putStrLn $ prettyItem now li

prettyItem :: UTCTime -> LastItem -> String
prettyItem now LastItem {..} =
    let timeStr = prettyTimestamp now lastItemScheduled
        timeAgoString = prettyTimeAuto now lastItemScheduled
    in case typedItemCase lastItemData of
           Left err -> unlines ["Invalid item:", err]
           Right i ->
               case i of
                   CaseTextItem t ->
                       unlines
                           [ concat [timeStr, " (", timeAgoString, ")"]
                           , T.unpack t
                           ]

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
    let year = (\(y, _, _) -> y) . toGregorian . utctDay
    in (if year now == year d
            then formatTime defaultTimeLocale "%A %B %e at %H:%M"
            else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M")
           d
