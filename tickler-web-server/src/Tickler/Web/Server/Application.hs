{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.Application where

import Yesod
import Yesod.Auth

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.APIDocs
import Tickler.Web.Server.Handler.Account
import Tickler.Web.Server.Handler.Add
import Tickler.Web.Server.Handler.Admin
import Tickler.Web.Server.Handler.Checkout
import Tickler.Web.Server.Handler.Delete
import Tickler.Web.Server.Handler.DeleteTriggereds
import Tickler.Web.Server.Handler.Edit
import Tickler.Web.Server.Handler.EmailTriggerResend
import Tickler.Web.Server.Handler.EmailTriggerVerify
import Tickler.Web.Server.Handler.Error
import Tickler.Web.Server.Handler.Home
import Tickler.Web.Server.Handler.Loopers
import Tickler.Web.Server.Handler.RetryTriggered
import Tickler.Web.Server.Handler.Tickles
import Tickler.Web.Server.Handler.Triggered
import Tickler.Web.Server.Handler.Triggers

mkYesodDispatch "App" resourcesApp
