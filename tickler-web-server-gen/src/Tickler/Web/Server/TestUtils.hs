{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.TestUtils
  ( ticklerWebServerSpec,
    ticklerWebServerAndDatabaseSpec,
    freeTicklerWebServerSpec,
    paidTicklerWebServerSpec,
    appSetupFunc,
    withExampleAccount,
    withExampleAccount_,
    withExampleAccountAndLogin,
    withExampleAccountAndLogin_,
    withAdminAccount,
    withAdminAccount_,
    withAdminAccountAndLogin,
    withAdminAccountAndLogin_,
    withFreshAccount,
    loginTo,
    logout,
    addItem,
    addItemRequestBuilder,
    editItem,
    editItemRequestBuilder,
    addEmailTrigger,
  )
where

import Control.Concurrent (newMVar)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import qualified Database.Persist.Sql as DB
import qualified Network.HTTP.Client as HTTP
import Path.IO
import Test.Syd
import Test.Syd.Path
import Test.Syd.Yesod
import Tickler.Client
import Tickler.Data.Gen ()
import qualified Tickler.Server.TestUtils as API
import Tickler.Web.Server.Application ()
import Tickler.Web.Server.Foundation
import Yesod.Auth

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

ticklerWebServerSpec :: YesodSpec App -> Spec
ticklerWebServerSpec = API.withTicklerServer . yesodSpecWithSiteSetupFunc' appSetupFunc

ticklerWebServerAndDatabaseSpec :: TestDef '[HTTP.Manager] (DB.ConnectionPool, YesodClient App) -> Spec
ticklerWebServerAndDatabaseSpec =
  API.withTicklerServerAndDatabase
    . setupAroundWith'
      ( \man (pool, cenv) -> do
          app <- appSetupFunc man cenv
          yc <- yesodClientSetupFunc man app
          pure (pool, yc)
      )

freeTicklerWebServerSpec :: YesodSpec App -> Spec
freeTicklerWebServerSpec = API.withFreeTicklerServer . yesodSpecWithSiteSetupFunc' appSetupFunc

paidTicklerWebServerSpec :: Int -> YesodSpec App -> Spec
paidTicklerWebServerSpec maxItems = API.withPaidTicklerServer maxItems . yesodSpecWithSiteSetupFunc' appSetupFunc

appSetupFunc :: HTTP.Manager -> ClientEnv -> SetupFunc App
appSetupFunc man (ClientEnv _ burl _) = do
  tdir <- tempDirSetupFunc "tickler-web-server"
  appSessionKeyFile <- resolveFile tdir "client_session_key.aes"
  let appLogLevel = LevelWarn
  let appHTTPManager = man
  let appStatic = myStatic
  appLoginTokens <- liftIO $ newMVar HM.empty
  let appAPIBaseUrl = burl
  let appTracking = Nothing
  let appVerification = Nothing
  let appPersistLogins = False
  let appDefaultIntrayUrl = Nothing
  pure App {..}

loginTo :: Username -> Text -> YesodExample App ()
loginTo username passphrase = do
  get $ AuthR LoginR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR loginFormPostTargetR
    addTokenFromCookie
    addPostParam "username" $ usernameText username
    addPostParam "passphrase" passphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right AddR

logout :: YesodExample App ()
logout = do
  request $ do
    setMethod methodPost
    setUrl $ AuthR LogoutR
    addTokenFromCookie
  statusIs 303
  _ <- followRedirect
  statusIs 200

withFreshAccount ::
  Username -> Text -> YesodExample App a -> YesodExample App a
withFreshAccount exampleUsername examplePassphrase func = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addTokenFromCookie
    addPostParam "username" $ usernameText exampleUsername
    addPostParam "passphrase" examplePassphrase
    addPostParam "passphrase-confirm" examplePassphrase
  statusIs 303
  loc <- getLocation
  liftIO $ loc `shouldBe` Right AddR
  result <- func
  logout
  pure result

withExampleAccount :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccount func = do
  let un = fromJust $ parseUsername "example"
      pw = "pass"
  withFreshAccount un pw $ func un pw

withExampleAccountAndLogin :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withExampleAccountAndLogin func =
  withExampleAccount $ \un_ p -> do
    loginTo un_ p
    func un_ p

withExampleAccount_ :: YesodExample App a -> YesodExample App a
withExampleAccount_ = withExampleAccount . const . const

withExampleAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withExampleAccountAndLogin_ = withExampleAccountAndLogin . const . const

withAdminAccount :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccount func = do
  let un = fromJust $ parseUsername "admin"
      pw = "admin"
  withFreshAccount un pw $ func un pw

withAdminAccount_ :: YesodExample App a -> YesodExample App a
withAdminAccount_ = withAdminAccount . const . const

withAdminAccountAndLogin :: (Username -> Text -> YesodExample App a) -> YesodExample App a
withAdminAccountAndLogin func =
  withAdminAccount $ \un_ p -> do
    loginTo un_ p
    func un_ p

withAdminAccountAndLogin_ :: YesodExample App a -> YesodExample App a
withAdminAccountAndLogin_ = withAdminAccountAndLogin . const . const

addItem :: Tickle -> YesodExample App ItemUUID
addItem tickle = do
  get AddR
  statusIs 200
  request $ addItemRequestBuilder tickle
  statusIs 303
  errOrLoc <- getLocation
  case errOrLoc of
    Right AddR -> do
      mResponse <- getResponse
      uuid <- case mResponse of
        Nothing -> liftIO $ expectationFailure "should have had a response by now."
        Just response -> do
          let headers = HTTP.responseHeaders response
          case lookup "tickler_uuid" headers of
            Nothing -> liftIO $ expectationFailure $ unlines ("Should have had a uuid header." : map show headers)
            Just bs -> case parseUUIDAsciiBytes bs of
              Nothing -> liftIO $ expectationFailure "Should have had a valid uuid header."
              Just uuid -> pure uuid
      _ <- followRedirect
      statusIs 200
      pure uuid
    route -> do
      liftIO $
        expectationFailure $
          unwords
            [ "Should have redirected to the edit page, but redirected here instead: ",
              show route
            ]

addItemRequestBuilder :: Tickle -> RequestBuilder App ()
addItemRequestBuilder tickle = do
  setUrl AddR
  tickleRequestBuilder tickle

editItem :: ItemUUID -> Tickle -> YesodExample App ()
editItem uuid tickle = do
  get AddR
  statusIs 200
  request $ editItemRequestBuilder uuid tickle
  statusIs 303
  locationShouldBe (EditR uuid)
  _ <- followRedirect
  statusIs 200

editItemRequestBuilder :: ItemUUID -> Tickle -> RequestBuilder App ()
editItemRequestBuilder uuid tickle = do
  setUrl $ EditR uuid
  tickleRequestBuilder tickle

tickleRequestBuilder :: Tickle -> RequestBuilder App ()
tickleRequestBuilder Tickle {..} = do
  setMethod methodPost
  addTokenFromCookie
  addPostParam "contents" tickleContent
  addPostParam "scheduled-day" $ T.pack $ formatTime defaultTimeLocale "%F" tickleScheduledDay
  forM_ tickleScheduledTime $ \tod ->
    addPostParam "scheduled-time" $ T.pack $ formatTime defaultTimeLocale "%H:%M" tod
  case tickleRecurrence of
    Nothing ->
      addPostParam "recurrence" "NoRecurrence"
    Just recurrence -> case recurrence of
      EveryDaysAtTime ds mtod -> do
        addPostParam "recurrence" "Days"
        addPostParam "days" $ T.pack $ show ds
        forM_ mtod $ \tod ->
          addPostParam "day-time-of-day" $ T.pack $ formatTime defaultTimeLocale "%H:%M" tod
      EveryMonthsOnDay ms md mtod -> do
        addPostParam "recurrence" "Months"
        addPostParam "months" $ T.pack $ show ms
        forM_ md $ \d -> addPostParam "day" $ T.pack $ show d
        forM_ mtod $ \tod ->
          addPostParam "month-time-of-day" $ T.pack $ formatTime defaultTimeLocale "%H:%M" tod

addEmailTrigger :: AddEmailTrigger -> YesodExample App ()
addEmailTrigger AddEmailTrigger {..} = do
  get TriggersR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl TriggerAddEmailR
    addTokenFromCookie
    addPostParam "email-address" $ emailAddressText addEmailTriggerEmailAddress
  statusIs 303
  locationShouldBe TriggersR
  _ <- followRedirect
  statusIs 200
