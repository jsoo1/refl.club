{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import AWS
import qualified Data.AWS.Error as AWS
import Control.Monad (forever, void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Data.AWS.Runtime (Context (..), ToText(..))
import qualified Data.AWS.Startup as Startup
import qualified Data.AWS.Runtime.Response as Response
import qualified Data.ByteString.UTF8 as BLU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Hreq.Client
import System.Environment (getEnv, setEnv)
import System.Exit
import qualified Index

main :: IO ()
main =
  Startup.env >>= either failOnInit runLambdaFunctions

-- | Fail on startup.
failOnInit :: AWS.ToError e => e -> IO ()
failOnInit e = do
  runtimeApi <- T.pack <$> getEnv "AWS_LAMBDA_RUNTIME_API"
  let initErrorUrl = HttpUrl runtimeApi $ runtimePath <> "/init/error"
  stat <- runHreq initErrorUrl $ initError e
  exitWith $ ExitFailure 1

runLambdaFunctions :: Startup.Env -> IO ()
runLambdaFunctions contextAWSEnv@Startup.Env {..} =
  case lambda (Startup.handler contextAWSEnv) of
    Left e -> failOnInit e
    Right Lambda {..} ->
      lambdaSetup contextAWSEnv >>= \case
        Left e -> failOnInit e
        Right contextEnv -> loop
          where
            loop = do
              (contextEvent :. contextEventHeaders :. Empty) <- runHreq (runtimeApiUrl runtimeApi) next
              let reqId' = lookup "Lambda-Runtime-Aws-Request-Id" contextEventHeaders
                  traceId' = lookup "Lambda-Runtime-Trace-Id" contextEventHeaders
                  runLambda (MonadLambda handler) = runExceptT . runReaderT handler
              reqId <- maybe (fail "Request Id not found in next invocation") pure reqId'
              traceId <- maybe (fail "Trace Id not found in next invocation") pure traceId'
              setEnv "_X_AMZN_TRACE_ID" $ BLU.toString traceId
              runLambda lambdaHandler Context {..} >>= \case
                Left e -> do
                  let err = AWS.TextErr e
                      reportError = AWS.error (T.decodeUtf8 reqId) (Response.Response err)
                  stat <- runHreq (invocationUrl runtimeApi) reportError
                  loop
                Right res -> do
                  let respondSuccess = AWS.response (T.decodeUtf8 reqId) res
                  stat <- runHreq (invocationUrl runtimeApi) respondSuccess
                  loop

lambda :: Monad m => Text -> Either Startup.Error (Lambda m)
lambda = \case
    "Index.handler" -> pure Index.lambda
    handler -> Left $ Startup.HandlerNotFound handler 
      
