{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.PDF where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Network.HTTP.Media ((//))
import Servant

data PDF

instance Accept PDF where
  contentType _ = "application" // "pdf"

instance MimeRender PDF ByteString where
  mimeRender _ = Data.ByteString.Lazy.fromStrict

instance MimeUnrender PDF ByteString where
  mimeUnrender _ = pure . Data.ByteString.Lazy.toStrict
