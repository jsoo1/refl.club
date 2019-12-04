module AWS
  ( module Data.AWS,
    module AWS.InitError,
    runtimeApiUrl
  )
where

import Data.AWS
import AWS.InitError
import Data.Text (Text)
import Hreq.Client

runtimeApiUrl :: Text -> BaseUrl
runtimeApiUrl api =
  HttpUrl api "2018-06-01/runtime"
