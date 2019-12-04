{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module AWS.InitError where

import Data.AWS
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Text (Text)
import Hreq.Client
import Hreq.Core.Client.BaseUrl

type InitError =
  "init"
    :> "error"
    :> ReqHeaders '["Lambda-Runtime-Function-Error-Type" := Text]
    :> ReqBody JSON Error
    :> PostJson StatusResponse

data InitErrorResponse
  = InitErrorAccepted StatusResponse
  | InitErrorForbidden ErrorResponse
  | InitContainerError

instance FromJSON InitErrorResponse where
  parseJSON x =
    (InitErrorAccepted <$> parseJSON x)
      <|> (InitErrorForbidden <$> parseJSON x)

initError :: (ToAWSError a, RunClient m) => a -> m StatusResponse
initError e =
  hreq @InitError $ "Unhandled" :. toAWSError e :. Empty
