{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Looper.TriggeredEmailConverterSpec (spec) where

import Data.Time
import Data.UUID as UUID
import Data.UUID.Typed as Typed
import TestImport
import Tickler.Data
import Tickler.Server.Looper.TriggeredEmailConverter
import Tickler.Server.OptParse.Types

spec :: Spec
spec = do
  let sets =
        TriggeredEmailConverterSettings
          { triggeredEmailConverterSetFromAddress = unsafeEmailAddress "tickler" "example.com",
            triggeredEmailConverterSetFromName = "tickler",
            triggeredEmailConverterSetWebHost = "https://tickler.example.com"
          }
  let ti =
        TriggeredItem
          { triggeredItemIdentifier = Typed.UUID $ UUID.fromWords 1 2 3 4,
            triggeredItemUserId = Typed.UUID $ UUID.fromWords 5 6 7 8,
            triggeredItemContents = "Hello world\nFoo bar",
            triggeredItemCreated = UTCTime (fromGregorian 2022 05 02) 36000,
            triggeredItemScheduledDay = fromGregorian 2022 05 03,
            triggeredItemScheduledTime = Just $ TimeOfDay 19 30 00,
            triggeredItemRecurrence = Nothing,
            triggeredItemTriggered = UTCTime (fromGregorian 2022 05 03) 31000
          }
  let urlRender = error "unused"

  it "produces the same subject as last time" $
    pureGoldenTextFile "test_resources/email/triggered.subject" $
      triggeredEmailSubject ti

  it "produces the same text content as last time" $
    pureGoldenTextFile "test_resources/email/triggered.text" $
      triggeredEmailTextContent sets ti urlRender

  it "produces the same text content as last time" $
    pureGoldenTextFile "test_resources/email/triggered.html" $
      triggeredEmailHtmlContent sets ti urlRender
