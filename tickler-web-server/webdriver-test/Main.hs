module Main where

import Spec
import Test.Syd
import Tickler.Web.Server.TestUtils.Webdriver

main :: IO ()
main = sydTest $ ticklerWebdriverSpec spec
