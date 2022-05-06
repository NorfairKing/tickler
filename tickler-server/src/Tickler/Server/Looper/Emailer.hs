{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.Emailer
  ( EmailerSettings (..),
    runEmailer,
  )
where

import Conduit
import Control.Lens
import Control.Monad.Trans.AWS as AWS (runAWST)
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import Import
import Network.AWS as AWS
import Network.AWS.SES
import System.IO
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

data EmailerSettings = EmailerSettings
  { emailerSetAWSCredentials :: AWS.Credentials
  }
  deriving (Show)

runEmailer :: EmailerSettings -> Looper ()
runEmailer EmailerSettings {..} = do
  acqEmailsToSendSource <- runDb $ selectSourceRes [EmailStatus ==. EmailUnsent] [Asc EmailScheduled]
  withAcquire acqEmailsToSendSource $ \emailsToSendSource -> do
    runConduit $ emailsToSendSource .| C.mapM_ (handleSingleEmail emailerSetAWSCredentials)

handleSingleEmail :: AWS.Credentials -> Entity Email -> Looper ()
handleSingleEmail awsCreds (Entity emailId email) =
  runDb $ do
    newStatus <- liftIO $ sendSingleEmail awsCreds email
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

sendSingleEmail :: AWS.Credentials -> Email -> IO (Either Text Text)
sendSingleEmail creds Email {..} = do
  lgr <- newLogger Debug stderr
  env <- set envLogger lgr <$> newEnv creds
  runResourceT . runAWST env . AWS.within Ireland $ do
    let txt = content emailTextContent
    let html = content emailHtmlContent
    let bod = body & bText ?~ txt & bHTML ?~ html
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
