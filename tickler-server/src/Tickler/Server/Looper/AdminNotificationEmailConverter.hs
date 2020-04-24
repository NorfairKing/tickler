{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Looper.AdminNotificationEmailConverter
  ( runAdminNotificationEmailConverter
  ) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time
import Database.Persist.Sqlite
import Import
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types
import Tickler.Server.OptParse.Types

runAdminNotificationEmailConverter :: AdminNotificationEmailConverterSettings -> Looper ()
runAdminNotificationEmailConverter vecs@AdminNotificationEmailConverterSettings {..} = do
  ves <- runDb $ selectList [AdminNotificationEmailEmail ==. Nothing] []
  forM_ ves $ \(Entity vid ve@AdminNotificationEmail {..}) -> do
    e <- makeAdminNotificationEmail vecs ve undefined
    runDb $ do
      eid <- insert e
      update vid [AdminNotificationEmailEmail =. Just eid]

makeAdminNotificationEmail ::
     AdminNotificationEmailConverterSettings
  -> AdminNotificationEmail
  -> Render Text
  -> Looper Email
makeAdminNotificationEmail vecs@AdminNotificationEmailConverterSettings {..} ve@AdminNotificationEmail {..} render = do
  now <- liftIO getCurrentTime
  pure
    Email
      { emailTo = adminNotificationEmailConverterSetToAddress
      , emailFrom = adminNotificationEmailConverterSetFromAddress
      , emailFromName = adminNotificationEmailConverterSetFromName
      , emailSubject = "Tickler Admin Notification"
      , emailTextContent = adminNotificationEmailTextContent vecs ve render
      , emailHtmlContent = adminNotificationEmailHtmlContent vecs ve render
      , emailStatus = EmailUnsent
      , emailSendError = Nothing
      , emailSesId = Nothing
      , emailScheduled = now
      , emailSendAttempt = Nothing
      }

adminNotificationEmailTextContent ::
     AdminNotificationEmailConverterSettings -> AdminNotificationEmail -> Render Text -> Text
adminNotificationEmailTextContent AdminNotificationEmailConverterSettings {..} AdminNotificationEmail {..} render =
  LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/admin-notification-email.txt") render

adminNotificationEmailHtmlContent ::
     AdminNotificationEmailConverterSettings -> AdminNotificationEmail -> Render Text -> Text
adminNotificationEmailHtmlContent AdminNotificationEmailConverterSettings {..} AdminNotificationEmail {..} render =
  LT.toStrict $ renderHtml $ $(hamletFile "templates/email/admin-notification-email.hamlet") render
