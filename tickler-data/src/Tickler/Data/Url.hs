{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Url
  ( BaseUrl (..),
  )
where

import Autodocodec
import Control.Exception
import Data.Char as Char
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Import
import Servant.Client.Core

instance Validity Scheme

instance Validity BaseUrl where
  validate burl@BaseUrl {..} =
    mconcat
      [ annotate baseUrlScheme "baseUrlScheme",
        annotate baseUrlHost "baseUrlHost",
        annotate baseUrlPort "baseUrlPort",
        annotate baseUrlPath "baseUrlPath",
        declare "The hostname is not empty" $ not $ null baseUrlHost,
        declare "The port is positive" $ baseUrlPort >= 0,
        declare "The port is less than 65536" $ baseUrlPort <= 65536,
        declare "The hostname does not contain spaces" $ not $ any Char.isSpace baseUrlHost,
        declare "The host is entirely within Latin1" $ all Char.isLatin1 baseUrlHost,
        declare "The path does not contain spaces" $ not $ any Char.isSpace baseUrlPath,
        declare "The path is entirely within Latin1" $ all Char.isLatin1 baseUrlPath,
        declare "The path does not start with a slash" $
          case baseUrlPath of
            ('/' : _) -> False
            _ -> True,
        declare
          ( unlines
              [ "Parsing the url after rendering it yields the same url",
                "expected: "
                  <> show
                    (showBaseUrl <$> (parseBaseUrl (showBaseUrl burl) :: Either SomeException BaseUrl)),
                "actual:   " <> showBaseUrl burl
              ]
          )
          $ parseBaseUrl (showBaseUrl burl) == Just burl
      ]

instance PersistField BaseUrl where
  toPersistValue = PersistText . T.pack . showBaseUrl
  fromPersistValue (PersistText t) = left (T.pack . show) $ parseBaseUrl $ T.unpack t
  fromPersistValue _ = Left "Invalid persist field type to parse BaseUrl"

instance PersistFieldSql BaseUrl where
  sqlType Proxy = SqlString

instance HasCodec BaseUrl where
  codec = bimapCodec (left show . parseBaseUrl) show codec
