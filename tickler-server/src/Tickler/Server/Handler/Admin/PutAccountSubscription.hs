{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Admin.PutAccountSubscription (serveAdminPutAccountSubscription) where

import Data.Time
import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminPutAccountSubscription :: AuthCookie -> Username -> UTCTime -> TicklerHandler NoContent
serveAdminPutAccountSubscription AuthCookie {..} username end = withAdminCreds authCookieUserUUID $ do
  mUserEntity <- runDB $ getBy (UniqueUsername username)
  case mUserEntity of
    Nothing -> throwError err404
    Just (Entity _ user) ->
      let uuid = userIdentifier user
       in runDB $
            void $
              upsertBy
                (UniqueSubscriptionUser uuid)
                ( Subscription
                    { subscriptionUser = uuid,
                      subscriptionEnd = end
                    }
                )
                [ SubscriptionEnd =. end
                ]
  pure NoContent
