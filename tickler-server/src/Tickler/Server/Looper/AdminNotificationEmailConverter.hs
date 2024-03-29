{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Looper.AdminNotificationEmailConverter
  ( AdminNotificationEmailConverterSettings (..),
    runAdminNotificationEmailConverter,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
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

data AdminNotificationEmailConverterSettings = AdminNotificationEmailConverterSettings
  { adminNotificationEmailConverterSetFromAddress :: !EmailAddress,
    adminNotificationEmailConverterSetFromName :: !Text,
    adminNotificationEmailConverterSetToAddress :: !EmailAddress,
    adminNotificationEmailConverterSetToName :: !Text,
    adminNotificationEmailConverterSetWebHost :: !Text
  }
  deriving (Show)

runAdminNotificationEmailConverter :: AdminNotificationEmailConverterSettings -> Looper ()
runAdminNotificationEmailConverter vecs = do
  acqAdminEmailSource <- runDB $ selectSourceRes [AdminNotificationEmailEmail ==. Nothing] []
  withAcquire acqAdminEmailSource $ \adminEmailSource ->
    runConduit $ adminEmailSource .| C.mapM_ (convertAdminEmail vecs)

convertAdminEmail :: AdminNotificationEmailConverterSettings -> Entity AdminNotificationEmail -> Looper ()
convertAdminEmail vecs (Entity vid ve) = do
  logInfoN $ T.pack $ unwords ["Converting admin notification email to email:", show vid]
  e <- makeAdminNotificationEmail vecs ve undefined
  runDB $ do
    eid <- insert e
    update vid [AdminNotificationEmailEmail =. Just eid]

makeAdminNotificationEmail ::
  AdminNotificationEmailConverterSettings ->
  AdminNotificationEmail ->
  Render Text ->
  Looper Email
makeAdminNotificationEmail vecs@AdminNotificationEmailConverterSettings {..} ve render = do
  now <- liftIO getCurrentTime
  pure
    Email
      { emailTo = adminNotificationEmailConverterSetToAddress,
        emailFrom = adminNotificationEmailConverterSetFromAddress,
        emailFromName = adminNotificationEmailConverterSetFromName,
        emailSubject = "Tickler Admin Notification",
        emailTextContent = adminNotificationEmailTextContent vecs ve render,
        emailHtmlContent = adminNotificationEmailHtmlContent vecs ve render,
        emailStatus = EmailUnsent,
        emailSendError = Nothing,
        emailSesId = Nothing,
        emailScheduled = now,
        emailSendAttempt = Nothing
      }

adminNotificationEmailTextContent ::
  AdminNotificationEmailConverterSettings -> AdminNotificationEmail -> Render Text -> Text
adminNotificationEmailTextContent AdminNotificationEmailConverterSettings {..} AdminNotificationEmail {..} render =
  LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/admin-notification-email.txt") render

adminNotificationEmailHtmlContent ::
  AdminNotificationEmailConverterSettings -> AdminNotificationEmail -> Render Text -> Text
adminNotificationEmailHtmlContent AdminNotificationEmailConverterSettings {..} AdminNotificationEmail {..} render =
  LT.toStrict $ renderHtml $ $(hamletFile "templates/email/admin-notification-email.hamlet") render
