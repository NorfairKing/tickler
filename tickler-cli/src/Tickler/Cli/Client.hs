module Tickler.Cli.Client where

import Import

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http

import Servant.Client

import Tickler.Cli.OptParse

runSingleClientOrErr :: ClientM a -> CliM (Maybe a)
runSingleClientOrErr func = do
    mErrOrRes <- runSingleClient func
    case mErrOrRes of
        Nothing -> pure Nothing
        Just errOrRes ->
            fmap Just $
            case errOrRes of
                Left err ->
                    liftIO $
                    die $
                    unlines
                        ["Error while contacting the tickler server:", show err]
                Right r -> pure r

runSingleClient :: ClientM a -> CliM (Maybe (Either ServantError a))
runSingleClient func = do
    mburl <- asks setBaseUrl
    case mburl of
        Nothing -> pure Nothing
        Just burl ->
            fmap Just $
            liftIO $ do
                man <- Http.newManager Http.tlsManagerSettings
                let env = ClientEnv man burl Nothing
                runClientM func env
