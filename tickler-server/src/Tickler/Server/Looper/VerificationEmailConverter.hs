{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.VerificationEmailConverter
    ( runVerificationEmailConverter
    ) where

import Import

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Data.Time

import Control.Monad.Logger
import Database.Persist.Sqlite

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Text.Shakespeare.Text

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runVerificationEmailConverter :: VerificationEmailConverterSettings -> Looper ()
runVerificationEmailConverter vecs@VerificationEmailConverterSettings {..} = do
    logInfoNS
        "TriggeredEmailConverter"
        "Starting converting VerificationEmails to Emails."
    ves <- runDb $ selectList [VerificationEmailEmail ==. Nothing] []
    forM_ ves $ \(Entity vid ve@VerificationEmail {..}) -> do
        e <- makeVerificationEmail vecs ve undefined
        runDb $ do
            eid <- insert e
            update vid [VerificationEmailEmail =. Just eid]
    logInfoNS
        "TriggeredEmailConverter"
        "Finished converting VerificationEmails to Emails."

makeVerificationEmail ::
       VerificationEmailConverterSettings
    -> VerificationEmail
    -> Render Text
    -> Looper Email
makeVerificationEmail vecs@VerificationEmailConverterSettings {..} ve@VerificationEmail {..} render = do
    now <- liftIO getCurrentTime
    pure
        Email
            { emailTo = verificationEmailTo
            , emailFrom = verificationEmailConverterSetFromAddress
            , emailFromName = verificationEmailConverterSetFromName
            , emailSubject = "Please verify your email trigger"
            , emailTextContent = verificationEmailTextContent vecs ve render
            , emailHtmlContent = verificationEmailHtmlContent vecs ve render
            , emailStatus = EmailUnsent
            , emailSendError = Nothing
            , emailSesId = Nothing
            , emailScheduled = now
            , emailSendAttempt = Nothing
            }

verificationEmailTextContent ::
       VerificationEmailConverterSettings
    -> VerificationEmail
    -> Render Text
    -> Text
verificationEmailTextContent VerificationEmailConverterSettings {..} VerificationEmail {..} render =
    let verificationLink =
            makeVerificationLink
                verificationEmailConverterSetWebHost
                verificationEmailTrigger
                verificationEmailKey
     in LT.toStrict $
        LTB.toLazyText $
        $(textFile "templates/email/verification-email.txt") render

verificationEmailHtmlContent ::
       VerificationEmailConverterSettings
    -> VerificationEmail
    -> Render Text
    -> Text
verificationEmailHtmlContent VerificationEmailConverterSettings {..} VerificationEmail {..} render =
    let verificationLink =
            makeVerificationLink
                verificationEmailConverterSetWebHost
                verificationEmailTrigger
                verificationEmailKey
     in LT.toStrict $
        renderHtml $
        $(hamletFile "templates/email/verification-email.hamlet") render

makeVerificationLink :: Text -> TriggerUUID -> EmailVerificationKey -> Text
makeVerificationLink host tuuid evk =
    T.concat
        [ "https://"
        , host
        , "/email-trigger/"
        , uuidText tuuid
        , "/verify/"
        , emailVerificationKeyText evk
        ]
