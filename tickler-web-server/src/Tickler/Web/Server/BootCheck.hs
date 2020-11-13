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
import Servant.Client

bootCheck :: Maybe BaseUrl -> IO ()
bootCheck mIntrayUrl = forM_ mIntrayUrl $ \url -> do
  man <- Http.newManager Http.tlsManagerSettings
  request <- Http.parseRequest $ showBaseUrl url
  response <- Http.httpLbs request man
  putStrLn "Trying out a https request to be able to crash early."
  putStrLn $ "The status code was: " ++ show (Http.statusCode $ Http.responseStatus response)
