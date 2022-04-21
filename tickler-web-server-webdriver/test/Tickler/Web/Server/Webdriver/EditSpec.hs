{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.EditSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec = do
  it "works for changing the contents of any tickle" $ \wte ->
    forAllValid $ \tickle ->
      runWebdriverTestM wte $
        editSpec (tickle {tickleContent = "original"}) (tickle {tickleContent = "new"})

  pending "works for changing the scheduled day of any tickle"
  pending "works for changing the scheduled time of any tickle"
  pending "works for adding daily recurrence to a tickle without recurrence"
  pending "works for adding monthly recurrence to a tickle without recurrence"
  pending "works for removing recurrence from a tickle with daily recurrence"
  pending "works for removing recurrence from a tickle with monthly recurrence"
  pending "works for making daily recurrence monthly"
  pending "works for making monthly recurrence daily"

editSpec :: Tickle -> Tickle -> WebdriverTestM App ()
editSpec originalTickle newTickle =
  driveAsNewUser dummyUser $ do
    uuid <- driveAddTickle originalTickle
    driveEditTickle uuid newTickle
    token <- getUserToken $ testUserUsername dummyUser
    ItemInfo {..} <- driveClientOrErr $ clientGetItem token uuid
    liftIO $ itemInfoContents `shouldBe` newTickle
