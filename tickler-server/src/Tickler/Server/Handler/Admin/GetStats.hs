{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Admin.GetStats
  ( serveAdminGetStats,
  )
where

import Data.Time
import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminGetStats :: AuthCookie -> TicklerHandler AdminStats
serveAdminGetStats AuthCookie {..} =
  withAdminCreds authCookieUserUUID $ do
    adminStatsNbUsers <- fromIntegral <$> runDB (count ([] :: [Filter User]))
    adminStatsNbTicklerItems <- fromIntegral <$> runDB (count ([] :: [Filter TicklerItem]))
    adminStatsNbTriggeredItems <- fromIntegral <$> runDB (count ([] :: [Filter TriggeredItem]))
    adminStatsNbSubscribers <-
      do
        us <- runDB $ selectList [] []
        fmap (fromIntegral . length . catMaybes) $
          forM us $
            \(Entity _ u) -> do
              ups <- getUserPaidStatus (userIdentifier u)
              pure $
                case ups of
                  HasNotPaid _ -> Nothing
                  HasPaid t -> Just t
                  NoPaymentNecessary -> Nothing
    now <- liftIO getCurrentTime
    let day :: NominalDiffTime
        day = 86400
    let activeUsers time =
          fmap fromIntegral $ runDB $ count [UserLastLogin >=. Just (addUTCTime (-time) now)]
    activeUsersDaily <- activeUsers day
    activeUsersWeekly <- activeUsers $ 7 * day
    activeUsersMonthly <- activeUsers $ 30 * day
    activeUsersYearly <- activeUsers $ 365 * day
    let adminStatsActiveUsers = ActiveUsers {..}
    pure AdminStats {..}
