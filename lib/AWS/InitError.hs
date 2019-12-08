{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AWS.InitError where

import Control.Applicative ((<|>))
import Data.AWS.Error as AWS
import qualified Data.AWS.Runtime as Runtime
import qualified Data.AWS.Startup as Startup
import Data.Aeson
import Data.Text (Text)
import Hreq.Client
import Hreq.Core.Client.BaseUrl

type InitError =
  "init"
    :> "error"
    :> ReqHeaders '["Lambda-Runtime-Function-Error-Type" := Text]
    :> ReqBody JSON Error
    :> PostJson Runtime.Status

data Response
  = InitErrorAccepted Runtime.Status
  | InitErrorForbidden Runtime.Error
  | InitContainerError

instance FromJSON AWS.InitError.Response where
  parseJSON x =
    (InitErrorAccepted <$> parseJSON x)
      <|> (InitErrorForbidden <$> parseJSON x)

initError :: (AWS.ToError a, RunClient m) => a -> m Runtime.Status
initError e =
  hreq @InitError $ "Unhandled" :. toAWSError e :. Empty
