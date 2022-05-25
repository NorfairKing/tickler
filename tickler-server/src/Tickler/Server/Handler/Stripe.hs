{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Server.Handler.Stripe
  ( PaidStatus (..),
    getUserPaidStatus,
  )
where

import Data.Time
import Database.Persist
import Import
import Servant
import Servant.Auth.Server
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.OptParse.Types
import Tickler.Server.Types

getUserPaidStatus :: AccountUUID -> TicklerHandler PaidStatus
getUserPaidStatus userId = do
  mss <- asks envMonetisation
  case mss of
    Nothing -> pure NoPaymentNecessary
    Just MonetisationSettings {..} -> do
      mu <- runDb $ getBy $ UniqueUserIdentifier userId
      case mu of
        Nothing -> throwAll err404
        Just (Entity _ User {..}) -> do
          isFreeloader <- asks ((userUsername `elem`) . envFreeloaders)
          if isFreeloader
            then pure NoPaymentNecessary
            else do
              isAdmin <- asks ((userUsername `elem`) . envAdmins)
              if isAdmin
                then pure NoPaymentNecessary
                else do
                  mSub <- hasSubscribed userId
                  case mSub of
                    Just u -> pure $ HasPaid u
                    Nothing -> do
                      c <- runDb $ count [TicklerItemUserId ==. userId]
                      pure $ HasNotPaid (monetisationSetMaxItemsFree - c)

hasSubscribed :: AccountUUID -> TicklerHandler (Maybe UTCTime)
hasSubscribed uuid = runDb $ fmap (fmap (subscriptionEnd . entityVal)) $ getBy $ UniqueSubscriptionUser uuid
