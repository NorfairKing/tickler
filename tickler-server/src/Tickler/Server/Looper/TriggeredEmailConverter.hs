{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Looper.TriggeredEmailConverter
    ( runTriggeredEmailConverter
    ) where

import Import

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

runTriggeredEmailConverter :: TriggeredEmailConverterSettings -> Looper ()
runTriggeredEmailConverter tess = do
    logInfoNS
        "TriggeredEmailConverter"
        "Starting converting TriggeredEmails to Emails."
    tes <- runDb $ selectList [TriggeredEmailEmail ==. Nothing] []
    tups <-
        fmap catMaybes $
        forM tes $ \(Entity tid TriggeredEmail {..}) -> do
            meti <-
                runDb $ getBy $ UniqueTriggeredItemIdentifier triggeredEmailItem
            meet <- runDb $ getBy $ UniqueEmailTrigger triggeredEmailTrigger
            case (,) <$> meti <*> meet of
                Nothing -> pure Nothing
                Just (Entity _ ti, Entity _ et) ->
                    (Just . (,) tid) <$> makeTriggeredEmail tess et ti undefined
    runDb $
        forM_ tups $ \(tid, e) -> do
            eid <- insert e
            -- This should be a transaction.
            update tid [TriggeredEmailEmail =. Just eid]
    logInfoNS
        "TriggeredEmailConverter"
        "Finished converting TriggeredEmails to Emails."

makeTriggeredEmail ::
       TriggeredEmailConverterSettings
    -> EmailTrigger
    -> TriggeredItem
    -> Renderer
    -> Looper Email
makeTriggeredEmail TriggeredEmailConverterSettings {..} EmailTrigger {..} ti@TriggeredItem {..} render = do
    now <- liftIO getCurrentTime
    pure
        Email
            { emailTo = emailTriggerAddress
            , emailFrom = triggeredEmailConverterSetFromAddress
            , emailFromName = triggeredEmailConverterSetFromName
            , emailSubject = "Tickle triggered"
            , emailTextContent = verificationEmailTextContent ti render
            , emailHtmlContent = verificationEmailHtmlContent ti render
            , emailStatus = EmailUnsent
            , emailSendError = Nothing
            , emailSesId = Nothing
            , emailScheduled = now
            , emailSendAttempt = Nothing
            }

verificationEmailTextContent :: TriggeredItem -> Renderer -> Text
verificationEmailTextContent TriggeredItem {..} render =
    LT.toStrict $
    LTB.toLazyText $ $(textFile "templates/email/triggered-email.txt") render

verificationEmailHtmlContent :: TriggeredItem -> Renderer -> Text
verificationEmailHtmlContent TriggeredItem {..} render =
    LT.toStrict $
    renderHtml $ $(hamletFile "templates/email/triggered-email.hamlet") render

type Renderer = Render Text
