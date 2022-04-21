{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.AddSpec (spec) where

import Control.Monad
import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec = do
  forM_ dummyTickles $ \tickle ->
    it "can add this tickle" $
      addSpec tickle

addSpec :: Tickle -> WebdriverTestM App ()
addSpec tickle = do
  driveAsNewUser dummyUser $ do
    uuid <- driveAddTickle tickle
    token <- getUserToken $ testUserUsername dummyUser
    ItemInfo {..} <- driveClientOrErr $ clientGetItem token uuid
    liftIO $ itemInfoContents `shouldBe` tickle
