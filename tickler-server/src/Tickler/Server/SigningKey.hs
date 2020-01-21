module Tickler.Server.SigningKey
  ( loadSigningKey
  ) where

import Import

import Crypto.JOSE.JWK (JWK)
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB

import Servant.Auth.Server as Auth

signingKeyFile :: IO (Path Abs File)
signingKeyFile = resolveFile' "signing-key.json"

storeSigningKey :: JWK -> IO ()
storeSigningKey key_ = do
  skf <- signingKeyFile
  LB.writeFile (toFilePath skf) (JSON.encodePretty key_)

loadSigningKey :: IO JWK
loadSigningKey = do
  skf <- signingKeyFile
  mErrOrKey <- forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath skf)
  case mErrOrKey of
    Nothing -> do
      key_ <- Auth.generateKey
      storeSigningKey key_
      pure key_
    Just (Left err) ->
      die $ unlines ["Failed to load signing key from file", fromAbsFile skf, "with error:", err]
    Just (Right r) -> pure r
