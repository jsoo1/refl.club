module AWS
  ( module AWS.InitError,
    module AWS.Invocation,
    module AWS.Lambda,
    module AWS.Startup,
    runtimePath,
    runtimeApiUrl,
    invocationUrl
  )
where

import AWS.InitError
import AWS.Invocation
import AWS.Lambda
import AWS.Startup
import Data.AWS.Startup
import Data.Text (Text)
import Hreq.Client

-- | Base path for all aws lambda endpoints
runtimePath :: Text
runtimePath = "2018-06-01/runtime"
               
-- | For use with the init error and next invocation endpoints
runtimeApiUrl :: RuntimeApi -> BaseUrl
runtimeApiUrl api =
  (HttpUrl (runtimeApiHost api) runtimePath)
  { baseUrlPort = runtimeApiPort api }

-- | Use to create the @BaseUrl for the invocation runtime endpoints (i.e. response and error)
-- | For use with the parameterized invocation endpoints (i.e. response and error).
invocationUrl :: RuntimeApi -> BaseUrl
invocationUrl api =
  runtimeUrl { baseUrlPath = baseUrlPath runtimeUrl <> "/invocation"}
  where
    runtimeUrl = runtimeApiUrl api
