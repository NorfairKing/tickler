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
       AuthResult AuthCookie -> ItemUUID -> TicklerHandler TypedItemInfo
serveGetItem (Authenticated AuthCookie {..}) id_ = do
    mIItem <- runDb $ getBy $ UniqueItemIdentifier id_
    case mIItem of
        Just item -> pure $ makeTicklerItemInfo $ entityVal item
        Nothing -> do
            mTItem <- runDb $ getBy $ UniqueTriggeredItemIdentifier id_
            case mTItem of
                Just item -> do
                    triggeredItemEns <-
                        runDb $
                        selectList
                            [ TriggeredIntrayItemItem ==.
                              triggeredItemIdentifier (entityVal item)
                            ]
                            []
                    triggeredEmailEns <-
                        runDb $
                        selectList
                            [ TriggeredEmailItem ==.
                              triggeredItemIdentifier (entityVal item)
                            ]
                            []
                    pure $
                        makeTriggeredItemInfo
                            (entityVal item)
                            (map entityVal triggeredItemEns)
                            (map entityVal triggeredEmailEns)
                Nothing -> throwError err404 {errBody = "Item not found."}
serveGetItem _ _ = throwAll err401
