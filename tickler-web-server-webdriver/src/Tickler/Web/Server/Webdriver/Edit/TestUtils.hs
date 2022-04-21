{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Edit.TestUtils where

import Control.Monad
import qualified Data.Text as T
import Data.Time
import Test.Syd
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Yesod
import Test.WebDriver
import Tickler.API
import Tickler.Web.Server.Foundation

driveEditTickle :: ItemUUID -> Tickle -> WebdriverTestM App ()
driveEditTickle uuid Tickle {..} = do
  findElem (ById "nav-tickles") >>= click
