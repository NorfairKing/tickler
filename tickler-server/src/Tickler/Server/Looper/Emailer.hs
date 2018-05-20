{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Emailer
    ( runEmailer
    ) where

import Import

import Control.Concurrent

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool
import Data.Time
import Database.Persist.Sqlite

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types

runEmailer :: EmailerSettings -> Looper ()
runEmailer EmailerSettings = do
    liftIO $ putStrLn "Running emailer: TODO"
    convertVerificationEmails
    sendEmails

convertVerificationEmails :: Looper ()
convertVerificationEmails = liftIO $ putStrLn "placeholder to convert verification emails to regular emails"
sendEmails :: Looper ()
sendEmails = liftIO $ putStrLn "placeholder to send emails"
