{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module AWS
  ( module AWS.InitError,
    module AWS.Invocation,
    module AWS.Lambda,
    runtimePath,
    runtimeApiUrl,
    InvocationUrlParams(..),
    InvocationEndpoint(..),
    invocationUrl
  )
where

import AWS.InitError
import AWS.Invocation
import AWS.Lambda
import Data.AWS.Startup
import Data.Text (Text)
import Hreq.Client

-- | Base path for all aws lambda endpoints
runtimePath :: Text
runtimePath = "2018-06-01/runtime"
               
-- | For use with the init error and next invocation endpoints
runtimeApiUrl :: RuntimeApiHost -> BaseUrl
runtimeApiUrl api =
  HttpUrl (unRuntimeApiHost api) runtimePath

-- | Use to create the @BaseUrl for the invocation runtime endpoints (i.e. response and error)
-- | Assemble after initial bootstrap environment variables are read.
data InvocationUrlParams
  = InvocationUrlParams
      { invocationUrlParamsHost :: RuntimeApiHost,
        invocationUrlParamsEndpoint :: InvocationEndpoint,
        invocationUrlParamsRequestId :: Text
      }

-- | The two available invocation urls: response and error
data InvocationEndpoint
  = InvocationError
  | InvocationResponse

-- | For use with the parameterized invocation endpoints (i.e. response and error).
invocationUrl :: InvocationUrlParams -> BaseUrl
invocationUrl InvocationUrlParams {..} =
  runtimeUrl
    { baseUrlPath =
        baseUrlPath runtimeUrl <> "/invocation"
          <> ("/" <> invocationUrlParamsRequestId)
          <> ("/" <> endpointText invocationUrlParamsEndpoint)
    }
  where
    runtimeUrl = runtimeApiUrl invocationUrlParamsHost
    endpointText = \case
      InvocationError -> "error"
      InvocationResponse -> "response"
