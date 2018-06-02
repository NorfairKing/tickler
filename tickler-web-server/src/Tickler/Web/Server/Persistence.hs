{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.Persistence
    ( readLogins
    , writeLogins
    ) where

import Import

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict (HashMap)
import qualified Data.Text.Encoding as TE

import Servant.Auth.Client (Token(..))

import Tickler.Client

loginsFile :: IO (Path Abs File)
loginsFile = resolveFile' "tickler-logins.json"

readLogins :: IO (Maybe (HashMap Username Token))
readLogins = do
    lf <- loginsFile
    mErrOrLogins <-
        forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath lf)
    case mErrOrLogins of
        Nothing -> pure Nothing
        Just (Left err) -> do
            putStrLn $
                unwords
                    [ "Failed to load logins from"
                    , fromAbsFile lf
                    , "with error:"
                    , err
                    ]
            pure Nothing
        Just (Right r) -> pure $ Just r

writeLogins :: HashMap Username Token -> IO ()
writeLogins m = do
    lf <- loginsFile
    LB.writeFile (toFilePath lf) (JSON.encodePretty m)

instance FromJSON Token where
    parseJSON =
        withText "Token" $ \t ->
            case Base16.decode $ TE.encodeUtf8 t of
                (h, "") -> pure $ Token h
                _ ->
                    fail
                        "Invalid token in JSON: could not decode from hex string"

instance ToJSON Token where
    toJSON (Token bs) =
        case TE.decodeUtf8' $ Base16.encode bs of
            Left _ ->
                error "Failed to decode hex string to text, should not happen."
            Right t -> JSON.String t
