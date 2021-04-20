{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.PgpKey where

import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.HTTP.Media ((//))
import Servant

data PgpKey

instance Accept PgpKey where
  contentType _ = "application" // "pgp-keys"

instance MimeRender PgpKey Text where
  mimeRender _ = Data.ByteString.Lazy.fromStrict . Text.encodeUtf8

instance MimeUnrender PgpKey Text where
  mimeUnrender _ bytes =
    case Text.decodeUtf8' (Data.ByteString.Lazy.toStrict bytes) of
      Left e -> Left $ show e
      Right t -> Right t
