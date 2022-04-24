{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.EditSpec (spec) where

import Control.Monad
import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec =
  forM_ dummyTickles $ \t1 ->
    describe ("From " <> show (tickleContent t1)) $
      forM_ dummyTickles $ \t2 ->
        when (t1 /= t2) $ editSpec t1 t2

editSpec :: Tickle -> Tickle -> WebdriverSpec App
editSpec originalTickle newTickle =
  it ("works for editing this tuple to be " <> show (tickleContent newTickle)) $ \wte ->
    let ctx = unlines ["original:", ppShow originalTickle, "new:", ppShow newTickle]
     in context ctx $
          runWebdriverTestM wte $
            driveAsNewUser dummyUser $ do
              uuid <- driveAddTickle originalTickle
              driveEditTickle uuid newTickle
              token <- getUserToken $ testUserUsername dummyUser
              ItemInfo {..} <- driveClientOrErr $ clientGetItem token uuid
              liftIO $ itemInfoContents `shouldBe` newTickle
