module Main where

import Spec
import Test.Syd
import Tickler.Web.Server.Webdriver.TestUtils

main :: IO ()
main = sydTest $ ticklerWebdriverSpec spec
