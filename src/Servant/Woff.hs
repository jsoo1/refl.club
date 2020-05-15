{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Woff where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media ((//))
import Servant

data Woff

data Woff2

instance Accept Woff where
  contentType _ = "font" // "woff"

instance Accept Woff2 where
  contentType _ = "font" // "woff2"

instance MimeRender Woff ByteString where
  mimeRender _ = id

instance MimeUnrender Woff ByteString where
  mimeUnrender _ = pure

instance MimeRender Woff2 ByteString where
  mimeRender _ = id

instance MimeUnrender Woff2 ByteString where
  mimeUnrender _ = pure
