{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.Application where

import Yesod
import Yesod.Auth

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler

mkYesodDispatch "App" resourcesApp
