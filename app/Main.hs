{-# LANGUAGE RecordWildCards #-}

module Main where

import AWS
import Control.Monad (forever, void)
import Data.AWS.Runtime (Context (..))
import qualified Data.AWS.Startup as Startup
import qualified Data.Text as T
import Data.Text (Text)
import Hreq.Client
import System.Environment (getEnv, setEnv)
import System.Exit
import qualified Index

main :: IO ()
main =
  Startup.env >>= either failOnInit runLambdaFunctions

-- | Fail on startup.
failOnInit :: Startup.Error -> IO ()
failOnInit e = do
  runtimeApi <- T.pack <$> getEnv "AWS_LAMBDA_RUNTIME_API"
  let initErrorUrl = HttpUrl runtimeApi $ runtimePath <> "/init/error"
  stat <- runHreq initErrorUrl $ initError e
  exitWith $ ExitFailure 1

runLambdaFunctions :: Startup.Env -> IO ()
runLambdaFunctions contextStartupEnv = do
  Lambda {..} <- lambda contextStartupEnv
  contextStartupSetup <- lambdaRunSetup contextStartupEnv
  void $ pure $ lambdaRunHandler $ Context {..}

lambda :: (Monad f, Monad m) => Startup.Env -> f (Lambda m)
lambda env =
  case Startup.handler env of
    "Index.handler" -> pure Index.lambda
    _ ->

      fail "no valid handler provided"
      
