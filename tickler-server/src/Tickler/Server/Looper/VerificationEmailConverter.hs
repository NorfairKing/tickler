{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Looper.VerificationEmailConverter
  ( runVerificationEmailConverter,
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
import Tickler.Server.OptParse.Types

runVerificationEmailConverter :: VerificationEmailConverterSettings -> Looper ()
runVerificationEmailConverter vecs@VerificationEmailConverterSettings {..} = do
  acqVerificationEmailSource <- runDb $ selectSourceRes [VerificationEmailEmail ==. Nothing] []
  withAcquire acqVerificationEmailSource $ \verificationEmailSource ->
    runConduit $ verificationEmailSource .| C.mapM_ (convertVerificationEmail vecs)

convertVerificationEmail :: VerificationEmailConverterSettings -> Entity VerificationEmail -> Looper ()
convertVerificationEmail vecs (Entity vid ve) = do
  email <- makeVerificationEmail vecs ve (error "unused")
  runDb $ do
    emailId <- insert email
    update vid [VerificationEmailEmail =. Just emailId]

makeVerificationEmail ::
  VerificationEmailConverterSettings -> VerificationEmail -> Render Text -> Looper Email
makeVerificationEmail vecs@VerificationEmailConverterSettings {..} ve@VerificationEmail {..} render = do
  now <- liftIO getCurrentTime
  pure
    Email
      { emailTo = verificationEmailTo,
        emailFrom = verificationEmailConverterSetFromAddress,
        emailFromName = verificationEmailConverterSetFromName,
        emailSubject = "Please verify your email trigger",
        emailTextContent = verificationEmailTextContent vecs ve render,
        emailHtmlContent = verificationEmailHtmlContent vecs ve render,
        emailStatus = EmailUnsent,
        emailSendError = Nothing,
        emailSesId = Nothing,
        emailScheduled = now,
        emailSendAttempt = Nothing
      }

verificationEmailTextContent ::
  VerificationEmailConverterSettings -> VerificationEmail -> Render Text -> Text
verificationEmailTextContent VerificationEmailConverterSettings {..} VerificationEmail {..} render =
  let verificationLink =
        makeVerificationLink
          verificationEmailConverterSetWebHost
          verificationEmailTrigger
          verificationEmailKey
   in LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/verification-email.txt") render

verificationEmailHtmlContent ::
  VerificationEmailConverterSettings -> VerificationEmail -> Render Text -> Text
verificationEmailHtmlContent VerificationEmailConverterSettings {..} VerificationEmail {..} render =
  let verificationLink =
        makeVerificationLink
          verificationEmailConverterSetWebHost
          verificationEmailTrigger
          verificationEmailKey
   in LT.toStrict $ renderHtml $ $(hamletFile "templates/email/verification-email.hamlet") render

makeVerificationLink :: Text -> TriggerUUID -> EmailVerificationKey -> Text
makeVerificationLink host tuuid evk =
  T.concat
    ["https://", host, "/email-trigger/", uuidText tuuid, "/verify/", emailVerificationKeyText evk]
