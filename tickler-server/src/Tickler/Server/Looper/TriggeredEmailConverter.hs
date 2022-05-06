{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Looper.TriggeredEmailConverter
  ( TriggeredEmailConverterSettings (..),
    runTriggeredEmailConverter,
    triggeredEmailSubject,
    triggeredEmailTextContent,
    triggeredEmailHtmlContent,
  )
where

import Conduit
import Data.Char as Char
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

data TriggeredEmailConverterSettings = TriggeredEmailConverterSettings
  { triggeredEmailConverterSetFromAddress :: !EmailAddress,
    triggeredEmailConverterSetFromName :: !Text,
    triggeredEmailConverterSetWebHost :: !Text
  }
  deriving (Show)

runTriggeredEmailConverter :: TriggeredEmailConverterSettings -> Looper ()
runTriggeredEmailConverter tess = do
  acqTriggeredEmailSource <- runDb $ selectSourceRes [TriggeredEmailEmail ==. Nothing] []
  withAcquire acqTriggeredEmailSource $ \triggeredEmailSource -> do
    runConduit $ triggeredEmailSource .| C.mapM_ (convertTriggeredEmail tess)

convertTriggeredEmail :: TriggeredEmailConverterSettings -> Entity TriggeredEmail -> Looper ()
convertTriggeredEmail tess (Entity tid TriggeredEmail {..}) = do
  meti <- runDb $ getBy $ UniqueTriggeredItemIdentifier triggeredEmailItem
  meet <- runDb $ getBy $ UniqueEmailTrigger triggeredEmailTrigger
  case (,) <$> meti <*> meet of
    Nothing -> pure ()
    Just (Entity _ ti, Entity _ et) -> do
      email <- makeTriggeredEmail tess et ti
      runDb $ do
        emailId <- insert email
        update tid [TriggeredEmailEmail =. Just emailId]

makeTriggeredEmail ::
  TriggeredEmailConverterSettings -> EmailTrigger -> TriggeredItem -> Looper Email
makeTriggeredEmail tecs@TriggeredEmailConverterSettings {..} EmailTrigger {..} ti = do
  now <- liftIO getCurrentTime
  pure
    Email
      { emailTo = emailTriggerAddress,
        emailFrom = triggeredEmailConverterSetFromAddress,
        emailFromName = triggeredEmailConverterSetFromName,
        emailSubject = triggeredEmailSubject ti,
        emailTextContent = triggeredEmailTextContent tecs ti,
        emailHtmlContent = triggeredEmailHtmlContent tecs ti,
        emailStatus = EmailUnsent,
        emailSendError = Nothing,
        emailSesId = Nothing,
        emailScheduled = now,
        emailSendAttempt = Nothing
      }

triggeredEmailSubject :: TriggeredItem -> Text
triggeredEmailSubject TriggeredItem {..} =
  T.pack $
    unwords
      [ "[Tickler]:",
        take 50 $
          filter (\c -> not (Char.isControl c) && c /= '\n' && c /= '\r') $
            T.unpack triggeredItemContents
      ]

triggeredEmailTextContent :: TriggeredEmailConverterSettings -> TriggeredItem -> Text
triggeredEmailTextContent TriggeredEmailConverterSettings {..} TriggeredItem {..} =
  LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/triggered-email.txt") (error "unused")

triggeredEmailHtmlContent :: TriggeredEmailConverterSettings -> TriggeredItem -> Text
triggeredEmailHtmlContent TriggeredEmailConverterSettings {..} TriggeredItem {..} =
  LT.toStrict $ renderHtml $ $(hamletFile "templates/email/triggered-email.hamlet") (error "unused")
