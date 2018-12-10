{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.Emailer
    ( runEmailer
    ) where

import Import

import Control.Lens
import Control.Monad.Logger
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import Network.AWS as AWS
import Network.AWS.SES
import System.IO

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

runEmailer :: EmailerSettings -> Looper ()
runEmailer EmailerSettings {..} = do
    logInfoNS "Emailer" "Starting to send emails."
    go
    logInfoNS "Emailer" "Finished sending emails."
  where
    go = do
        list <-
            runDb $
            selectFirst [EmailStatus ==. EmailUnsent] [Asc EmailScheduled]
        case list of
            Nothing -> pure ()
            Just (Entity emailId email) -> do
                runDb $ update emailId [EmailStatus =. EmailSending]
                newStatus <-
                    liftIO $ sendSingleEmail emailerSetAWSCredentials email
                now <- liftIO getCurrentTime
                runDb $
                    update emailId $
                    case newStatus of
                        Right hid ->
                            [ EmailStatus =. EmailSent
                            , EmailSesId =. Just hid
                            , EmailSendAttempt =. Just now
                            ]
                        Left err ->
                            [ EmailStatus =. EmailError
                            , EmailSendError =. Just err
                            , EmailSendAttempt =. Just now
                            ]
                go -- Go on until there are no more emails to be sent.

sendSingleEmail :: AWS.Credentials -> Email -> IO (Either Text Text)
sendSingleEmail creds Email {..} = do
    lgr <- newLogger Debug stderr
    env <- set envLogger lgr <$> newEnv creds
    runResourceT . runAWST env . AWS.within Ireland $ do
        let txt = content emailTextContent
        let html = content emailHtmlContent
        let bod = body & bText .~ Just txt & bHTML .~ Just html
        let sub = content emailSubject
        let mesg = message sub bod
        let dest = destination & dToAddresses .~ [emailAddressText emailTo]
        let req = sendEmail (emailAddressText emailFrom) dest mesg
        errOrResp <- trying _ServiceError (AWS.send req)
        pure $
            case errOrResp of
                Left err -> Left $ T.pack $ show err
                Right resp ->
                    case resp ^. sersResponseStatus of
                        200 -> Right $ resp ^. sersMessageId
                        _ -> Left "Error while sending email."
