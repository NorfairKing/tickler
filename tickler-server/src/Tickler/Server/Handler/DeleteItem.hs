{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.DeleteItem
    ( serveDeleteItem
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveDeleteItem :: AuthResult AuthCookie -> ItemUUID -> TicklerHandler NoContent
serveDeleteItem (Authenticated AuthCookie {..}) id_ = do
    runDb . deleteBy $ UniqueItemIdentifier id_
    runDb . deleteBy $ UniqueTriggeredItemIdentifier id_
    pure NoContent
serveDeleteItem _ _ = throwAll err401
