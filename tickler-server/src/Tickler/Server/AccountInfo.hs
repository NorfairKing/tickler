{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.AccountInfo
  ( getAccountInfoForUser,
  )
where

import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Stripe
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

getAccountInfoForUser :: User -> TicklerHandler AccountInfo
getAccountInfoForUser User {..} = do
  admins <- asks envAdmins
  tic <- runDb $ count ([TicklerItemUserId ==. userIdentifier] :: [Filter TicklerItem])
  tric <- runDb $ count ([TriggeredItemUserId ==. userIdentifier] :: [Filter TriggeredItem])
  ups <- getUserPaidStatus userIdentifier
  pure
    AccountInfo
      { accountInfoUUID = userIdentifier,
        accountInfoUsername = userUsername,
        accountInfoCreated = userCreated,
        accountInfoLastLogin = userLastLogin,
        accountInfoAdmin = userUsername `elem` admins,
        accountInfoTicklerItemCount = tic,
        accountInfoTriggeredItemCount = tric,
        accountInfoStatus = ups
      }
