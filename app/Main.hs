module Main where

import AWS
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text (Text)
import Hreq.Client
import Hreq.Core.Client.BaseUrl
import Startup
import System.Environment (getEnv, setEnv)
import System.Exit

main :: IO ()
main = startupEnv >>= \case
  Left e -> do
    runtimeApi <- runtimeApiUrl . T.pack <$> getEnv "AWS_LAMBDA_RUNTIME_API"
    stat <- runHreq runtimeApi $ initError e
    exitWith $ ExitFailure 1
  Right env -> print "hi"
