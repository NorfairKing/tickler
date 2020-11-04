{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.BootCheck
  ( bootCheck,
  )
where

import Import
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import qualified Network.HTTP.Types as Http

bootCheck :: IO ()
bootCheck = do
  man <- Http.newManager Http.tlsManagerSettings
  request <- Http.parseRequest "https://google.com"
  response <- Http.httpLbs request man
  putStrLn "Trying out a https request to be able to crash early."
  putStrLn $ "The status code was: " ++ show (Http.statusCode $ Http.responseStatus response)
