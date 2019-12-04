module AWS
  ( module AWS.Data,
    module AWS.InitError,
  )
where

import AWS.Data
import AWS.InitError
import Data.Text (Text)
import Hreq.Client

runtimeApiUrl :: Text -> BaseUrl
runtimeApiUrl api =
  HttpUrl api "2018-06-01/runtime"
