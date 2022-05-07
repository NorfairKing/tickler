{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Looper.VerificationEmailConverter
  ( VerificationEmailConverterSettings (..),
    runVerificationEmailConverter,
    verificationEmailSubject,
    verificationEmailTextContent,
    verificationEmailHtmlContent,
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

data VerificationEmailConverterSettings = VerificationEmailConverterSettings
  { verificationEmailConverterSetFromAddress :: !EmailAddress,
    verificationEmailConverterSetFromName :: !Text,
    verificationEmailConverterSetWebHost :: !Text
  }
  deriving (Show)

runVerificationEmailConverter :: VerificationEmailConverterSettings -> Looper ()
runVerificationEmailConverter vecs@VerificationEmailConverterSettings {..} = do
  acqVerificationEmailSource <- runDb $ selectSourceRes [VerificationEmailEmail ==. Nothing] []
  withAcquire acqVerificationEmailSource $ \verificationEmailSource ->
    runConduit $ verificationEmailSource .| C.mapM_ (convertVerificationEmail vecs)

convertVerificationEmail :: VerificationEmailConverterSettings -> Entity VerificationEmail -> Looper ()
convertVerificationEmail vecs (Entity vid ve) = do
  logInfoN $ T.pack $ unwords ["Converting verification email to email:", show vid]
  email <- makeVerificationEmail vecs ve
  runDb $ do
    emailId <- insert email
    update vid [VerificationEmailEmail =. Just emailId]

makeVerificationEmail ::
  VerificationEmailConverterSettings -> VerificationEmail -> Looper Email
makeVerificationEmail vecs@VerificationEmailConverterSettings {..} ve@VerificationEmail {..} = do
  now <- liftIO getCurrentTime
  pure
    Email
      { emailTo = verificationEmailTo,
        emailFrom = verificationEmailConverterSetFromAddress,
        emailFromName = verificationEmailConverterSetFromName,
        emailSubject = verificationEmailSubject,
        emailTextContent = verificationEmailTextContent vecs ve,
        emailHtmlContent = verificationEmailHtmlContent vecs ve,
        emailStatus = EmailUnsent,
        emailSendError = Nothing,
        emailSesId = Nothing,
        emailScheduled = now,
        emailSendAttempt = Nothing
      }

verificationEmailSubject :: Text
verificationEmailSubject = "Please verify your email trigger"

verificationEmailTextContent ::
  VerificationEmailConverterSettings -> VerificationEmail -> Text
verificationEmailTextContent VerificationEmailConverterSettings {..} VerificationEmail {..} =
  let verificationLink =
        makeVerificationLink
          verificationEmailConverterSetWebHost
          verificationEmailTrigger
          verificationEmailKey
   in LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/verification-email.txt") (error "unused")

verificationEmailHtmlContent ::
  VerificationEmailConverterSettings -> VerificationEmail -> Text
verificationEmailHtmlContent VerificationEmailConverterSettings {..} VerificationEmail {..} =
  let verificationLink =
        makeVerificationLink
          verificationEmailConverterSetWebHost
          verificationEmailTrigger
          verificationEmailKey
   in LT.toStrict $ renderHtml $ $(hamletFile "templates/email/verification-email.hamlet") (error "unused")

makeVerificationLink :: Text -> TriggerUUID -> EmailVerificationKey -> Text
makeVerificationLink host tuuid evk =
  T.concat
    ["https://", host, "/email-trigger/", uuidText tuuid, "/verify/", emailVerificationKeyText evk]
