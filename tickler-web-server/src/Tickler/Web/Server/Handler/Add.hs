{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Add
    ( getAddR
    ) where

import Import

import Yesod

import Tickler.Web.Server.Foundation

getAddR :: Handler Html
getAddR =
    withLogin $ \_ -> do
        token <- genToken
        withNavBar $(widgetFile "add")
