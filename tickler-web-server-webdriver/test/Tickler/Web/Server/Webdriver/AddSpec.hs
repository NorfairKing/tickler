{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.AddSpec (spec) where

import Control.Monad
import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec = do
  forM_ dummyTickles $ \tickle ->
    addSpec tickle

addSpec :: Tickle -> WebdriverSpec App
addSpec tickle = do
  it "can add this tickle" $ \wte ->
    context ("Tickle to add: " <> ppShow tickle) $
      runWebdriverTestM wte $
        driveAsNewUser dummyUser $ do
          uuid <- driveAddTickle tickle
          token <- getUserToken $ testUserUsername dummyUser
          ItemInfo {..} <- driveClientOrErr $ clientGetItem token uuid
          liftIO $ itemInfoContents `shouldBe` tickle
