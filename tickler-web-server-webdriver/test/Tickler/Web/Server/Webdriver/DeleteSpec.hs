{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Webdriver.DeleteSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec =
  it "can delete this dummy tickle" $
    driveAsNewUser dummyUser $ do
      uuid <- driveAddTickle dummyTickle
      driveDeleteTickle uuid
      token <- getUserToken $ testUserUsername dummyUser
      errOrInfo <- driveClient $ clientGetItem token uuid
      liftIO $ case errOrInfo of
        Right itemInfo ->
          expectationFailure $
            unwords
              [ "should not have succeeded, but got this item info",
                ppShow itemInfo
              ]
        Left err -> case err of
          FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
          _ -> expectationFailure $ unwords ["Unexpected error:", show err]
