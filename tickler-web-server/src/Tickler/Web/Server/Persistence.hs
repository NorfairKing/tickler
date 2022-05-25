{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.Persistence
  ( readLogins,
    writeLogins,
  )
where

import Autodocodec
import Data.Aeson as JSON (FromJSON, ToJSON, eitherDecode)
import Data.Aeson.Encode.Pretty as JSON (encodePretty)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict (HashMap)
import qualified Data.Text.Encoding as TE
import Import
import Servant.Auth.Client (Token (..))
import Tickler.Client

loginsFile :: IO (Path Abs File)
loginsFile = resolveFile' "tickler-logins.json"

readLogins :: IO (Maybe (HashMap Username Token))
readLogins = do
  lf <- loginsFile
  mErrOrLogins <- forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath lf)
  case mErrOrLogins of
    Nothing -> pure Nothing
    Just (Left err) -> do
      putStrLn $ unwords ["Failed to load logins from", fromAbsFile lf, "with error:", err]
      pure Nothing
    Just (Right r) -> pure $ Just r

writeLogins :: HashMap Username Token -> IO ()
writeLogins m = do
  lf <- loginsFile
  LB.writeFile (toFilePath lf) (JSON.encodePretty m)

instance HasCodec Token where
  codec = bimapCodec f g codec
    where
      f t =
        case Base16.decode $ TE.encodeUtf8 t of
          Right h -> pure $ Token h
          Left err -> Left err
      g (Token bs) =
        case TE.decodeUtf8' $ Base16.encode bs of
          Left _ -> error "Failed to decode hex string to text, should not happen."
          Right t -> t

deriving via (Autodocodec Token) instance (FromJSON Token)

deriving via (Autodocodec Token) instance (ToJSON Token)
