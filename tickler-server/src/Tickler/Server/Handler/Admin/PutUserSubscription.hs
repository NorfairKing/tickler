{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Admin.PutUserSubscription
  ( serveAdminPutUserSubscription,
  )
where

import Data.Time
import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminPutUserSubscription :: AuthCookie -> Username -> UTCTime -> TicklerHandler NoContent
serveAdminPutUserSubscription AuthCookie {..} username end = withAdminCreds authCookieUserUUID $ do
  mUserEntity <- runDb $ getBy (UniqueUsername username)
  case mUserEntity of
    Nothing -> throwError err404
    Just (Entity _ user) ->
      let uuid = userIdentifier user
       in runDb $
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
