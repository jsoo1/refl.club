module AWS
  ( module Data.AWS,
    module AWS.InitError,
    module AWS.Startup,
    runtimeApiUrl
  )
where

import Data.AWS
import AWS.InitError
import AWS.Startup
import Data.Text (Text)
import Hreq.Client

runtimeApiUrl :: Text -> BaseUrl
runtimeApiUrl api =
  HttpUrl api "2018-06-01/runtime"
