{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.Emailer
  ( runEmailer,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.SES.SendEmail
import qualified Amazonka.SES.SendEmail as SES
import qualified Amazonka.SES.Types as SES
import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import Import
import qualified System.IO as IO
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runEmailer :: Looper ()
runEmailer = do
  acqEmailsToSendSource <- runDB $ selectSourceRes [EmailStatus ==. EmailUnsent] [Asc EmailScheduled]
  withAcquire acqEmailsToSendSource $ \emailsToSendSource -> do
    runConduit $ emailsToSendSource .| C.mapM_ handleSingleEmail

handleSingleEmail :: Entity Email -> Looper ()
handleSingleEmail (Entity emailId email) = do
  logInfoN $ T.pack $ unwords ["Sending email email:", show $ fromSqlKey emailId]
  runDB $ do
    newStatus <- liftIO $ sendSingleEmail email
    now <- liftIO getCurrentTime
    update emailId $
      case newStatus of
        Right hid ->
          [ EmailStatus =. EmailSent,
            EmailSesId =. Just hid,
            EmailSendAttempt =. Just now
          ]
        Left err ->
          [ EmailStatus =. EmailError,
            EmailSendError =. Just err,
            EmailSendAttempt =. Just now
          ]

sendSingleEmail :: Email -> IO (Either Text Text)
sendSingleEmail Email {..} = do
  logger <- AWS.newLogger AWS.Debug IO.stdout
  discoveredEnv <- liftIO $ AWS.newEnv AWS.discover
  let awsEnv =
        discoveredEnv
          { AWS.logger = logger,
            AWS.region = AWS.Ireland
          }

  let textBody = SES.newContent emailTextContent
  let htmlBody = SES.newContent emailHtmlContent
  let body = SES.newBody {SES.html = Just textBody, SES.text = Just htmlBody}
  let subject = SES.newContent emailSubject
  let message = SES.newMessage subject body
  let destination = SES.newDestination {SES.toAddresses = Just [emailAddressText emailTo]}
  let request = SES.newSendEmail (emailAddressText emailFrom) destination message

  errOrResp <- runResourceT $ AWS.sendEither awsEnv request

  pure $
    case errOrResp of
      Left err -> Left $ T.pack $ show err
      Right response ->
        case SES.httpStatus response of
          200 -> Right $ Amazonka.SES.SendEmail.messageId response
          _ -> Left "Error while sending email."
