{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Looper.TriggeredEmailConverter
  ( runTriggeredEmailConverter,
  )
where

import Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
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

runTriggeredEmailConverter :: TriggeredEmailConverterSettings -> Looper ()
runTriggeredEmailConverter tess = do
  tes <- runDb $ selectList [TriggeredEmailEmail ==. Nothing] []
  tups <-
    fmap catMaybes $
      forM tes $
        \(Entity tid TriggeredEmail {..}) -> do
          meti <- runDb $ getBy $ UniqueTriggeredItemIdentifier triggeredEmailItem
          meet <- runDb $ getBy $ UniqueEmailTrigger triggeredEmailTrigger
          case (,) <$> meti <*> meet of
            Nothing -> pure Nothing
            Just (Entity _ ti, Entity _ et) ->
              Just . (,) tid <$> makeTriggeredEmail tess et ti undefined
  runDb $
    forM_ tups $
      \(tid, e) -> do
        eid <- insert e
        -- FIXME This should be a transaction.
        update tid [TriggeredEmailEmail =. Just eid]

makeTriggeredEmail ::
  TriggeredEmailConverterSettings -> EmailTrigger -> TriggeredItem -> Render Text -> Looper Email
makeTriggeredEmail tecs@TriggeredEmailConverterSettings {..} EmailTrigger {..} ti@TriggeredItem {..} render = do
  now <- liftIO getCurrentTime
  pure
    Email
      { emailTo = emailTriggerAddress,
        emailFrom = triggeredEmailConverterSetFromAddress,
        emailFromName = triggeredEmailConverterSetFromName,
        emailSubject =
          "[Tickler]: "
            <> case triggeredItemType of
              TextItem ->
                let textContents = TE.decodeUtf8 triggeredItemContents
                 in T.take 50 $
                      T.filter (\c -> not (Char.isControl c) && c /= '\n' && c /= '\r') textContents,
        emailTextContent = triggeredEmailTextContent tecs ti render,
        emailHtmlContent = triggeredEmailHtmlContent tecs ti render,
        emailStatus = EmailUnsent,
        emailSendError = Nothing,
        emailSesId = Nothing,
        emailScheduled = now,
        emailSendAttempt = Nothing
      }

triggeredEmailTextContent :: TriggeredEmailConverterSettings -> TriggeredItem -> Render Text -> Text
triggeredEmailTextContent TriggeredEmailConverterSettings {..} TriggeredItem {..} render =
  case triggeredItemType of
    TextItem ->
      let textContents = TE.decodeUtf8 triggeredItemContents
       in LT.toStrict $ LTB.toLazyText $ $(textFile "templates/email/triggered-email.txt") render

triggeredEmailHtmlContent :: TriggeredEmailConverterSettings -> TriggeredItem -> Render Text -> Text
triggeredEmailHtmlContent TriggeredEmailConverterSettings {..} TriggeredItem {..} render =
  case triggeredItemType of
    TextItem ->
      let textContents = TE.decodeUtf8 triggeredItemContents
       in LT.toStrict $ renderHtml $ $(hamletFile "templates/email/triggered-email.hamlet") render
