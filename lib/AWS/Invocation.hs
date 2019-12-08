{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AWS.Invocation where

import Data.AWS.Runtime
import Data.Text (Text)
import Hreq.Client
import Hreq.Core.Client.BaseUrl

type Next =
  "invocation"
    :> "next"
    :> Get
         '[ ResBody JSON Event,
            ResHeaders
              '[ "Lambda-Runtime-Aws-Request-Id" := Text,
                 "Lambda-Runtime-Trace-Id" := Text,
                 "Lambda-Runtime-Client-Context" := Text,
                 "Lambda-Runtime-Cognito-Identity" := Text,
                 "Lambda-Runtime-Deadline-Ms" := Text,
                 "Lambda-Runtime-Invoked-Function-Arn" := Text
               ]
          ]

-- | For use with @runtimeApiUrl.
next :: RunClient m => m (Hlist '[Event, [Header]])
next =
  hreq @Next Empty
