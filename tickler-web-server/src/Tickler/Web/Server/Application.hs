{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.Application where

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler
import Yesod
import Yesod.Auth

mkYesodDispatch "App" resourcesApp
