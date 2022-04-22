{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.EditSpec (spec) where

import Control.Monad
import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec = do
  let tickleTuples :: [(Tickle, Tickle)]
      tickleTuples = do
        t1 <- dummyTickles
        t2 <- dummyTickles
        if t1 == t2
          then []
          else pure (t1, t2)

  forM_ tickleTuples $ \(original, new) ->
    editSpec original new

editSpec :: Tickle -> Tickle -> WebdriverSpec App
editSpec originalTickle newTickle =
  it "works for editing this tuple to be this other tuple" $ \wte ->
    let ctx = unlines ["original:", ppShow originalTickle, "new:", ppShow newTickle]
     in context ctx $
          runWebdriverTestM wte $
            driveAsNewUser dummyUser $ do
              uuid <- driveAddTickle originalTickle
              driveEditTickle uuid newTickle
              token <- getUserToken $ testUserUsername dummyUser
              ItemInfo {..} <- driveClientOrErr $ clientGetItem token uuid
              liftIO $ itemInfoContents `shouldBe` newTickle
