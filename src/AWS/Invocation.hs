{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AWS.Invocation where

import Data.AWS
import Data.Text (Text)
import Hreq.Client

type Next =
  "invocation"
    :> "next"
    :> Get
         '[ ResBody JSON EventResponse,
            ResHeaders
              '[ "Lambda-Runtime-Aws-Request-Id" := Text,
                 "Lambda-Runtime-Trace-Id" := Text,
                 "Lambda-Runtime-Client-Context" := Text,
                 "Lambda-Runtime-Cognito-Identity" := Text,
                 "Lambda-Runtime-Deadline-Ms" := Text,
                 "Lambda-Runtime-Invoked-Function-Arn" := Text
               ]
          ]

next :: RunClient m => m (Hlist '[EventResponse, [Header]])
next =
  hreq @Next Empty
