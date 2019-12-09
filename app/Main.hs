{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import AWS
import qualified Data.AWS.Error as AWS
import Control.Monad (forever, void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as AE
import Data.AWS.Runtime (Context (..), ToText(..))
import qualified Data.AWS.Startup as Startup
import qualified Data.AWS.Runtime.Response as Response
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.UTF8 as BLU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Hreq.Client
import System.Environment (getEnv, setEnv)
import System.Exit
import System.IO
import qualified Index

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  AWS.env >>= either failOnInit runLambdaFunctions

-- | Fail on startup.
failOnInit :: AWS.ToError e => e -> IO ()
failOnInit e = do
  runtimeApi <- T.pack <$> getEnv "AWS_LAMBDA_RUNTIME_API"
  let initErrorUrl = HttpUrl runtimeApi $ runtimePath <> "/init/error"
  BLC.putStrLn $ AE.encode $ AWS.toError e
  stat <- runHreq initErrorUrl $ initError e
  BLC.putStrLn $ AE.encode stat
  exitWith $ ExitFailure 1

runLambdaFunctions :: Startup.Env -> IO ()
runLambdaFunctions env =
  either failOnInit (lambdaLoop env) $ lambda (Startup.handler env)

lambdaLoop :: Startup.Env -> Lambda IO -> IO ()
lambdaLoop contextAWSEnv@Startup.Env {..} lambdaFn@Lambda {..} =
  lambdaSetup contextAWSEnv >>= either failOnInit loop
    where
      loop contextEnv = do
        (contextEvent :. contextEventHeaders :. Empty) <-
          runHreq (runtimeApiUrl runtimeApi) next
        reqId <-
          maybe (fail "Request Id not found in next invocation") pure
          $ lookup "Lambda-Runtime-Aws-Request-Id" contextEventHeaders
        traceId <-
          maybe (fail "Trace Id not found in next invocation") pure
          $ lookup "Lambda-Runtime-Trace-Id" contextEventHeaders
        setEnv "_X_AMZN_TRACE_ID" $ BLU.toString traceId
        runLambda lambdaHandler Context {..} >>= \case
          Left e -> do
            stat <- runHreq (invocationUrl runtimeApi) reportError
            loop contextEnv
            where
              err = Response.Response $ AWS.TextErr e
              reportError = AWS.error (T.decodeUtf8 reqId) err
          Right res -> do
            stat <- runHreq (invocationUrl runtimeApi) respondSuccess
            loop contextEnv
            where
              respondSuccess = AWS.response (T.decodeUtf8 reqId) res


lambda :: Monad m => Text -> Either Startup.Error (Lambda m)
lambda = \case
    "Index.handler" -> pure Index.lambda
    handler -> Left $ Startup.HandlerNotFound handler 
      
