{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.CSS where

import qualified Data.ByteString.Lazy
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Network.HTTP.Media ((//))
import Servant

data CSS

instance Accept CSS where
  contentType _ = "text" // "css"

instance MimeRender CSS Text where
  mimeRender _ = Data.ByteString.Lazy.fromStrict . Text.encodeUtf8

instance MimeUnrender CSS Text where
  mimeUnrender _ bytes =
    case Text.decodeUtf8' (Data.ByteString.Lazy.toStrict bytes) of
      Left e -> Left $ show e
      Right t -> Right t
