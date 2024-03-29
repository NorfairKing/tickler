{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Constants where

import Import
import Language.Haskell.TH
import System.Environment

development :: Bool
development =
  $( do
       md <- runIO $ lookupEnv "DEVELOPMENT"
       fmap ConE $ case md of
         Nothing -> pure 'False
         Just _ -> do
           runIO $ putStrLn "WARNING: BUILDING TICKLER_WEB_SERVER IN DEVELOPMENT MODE"
           pure 'True
   )
