{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetItem
    ( serveGetItem
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Item
import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetItem ::
       AuthResult AuthCookie -> ItemUUID -> TicklerHandler (ItemInfo TypedItem)
serveGetItem (Authenticated AuthCookie {..}) id_ = do
    mitem <- runDb $ getBy $ UniqueIdentifier id_ authCookieUserUUID
    case mitem of
        Nothing -> throwError err404 {errBody = "Item not found."}
        Just item -> pure $ makeItemInfo $ entityVal item
serveGetItem _ _ = throwAll err401
