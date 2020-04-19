{-# LANGUAGE LambdaCase #-}

module Main where

import qualified AWS
import Data.AWS.Startup as Startup
import qualified Data.AWS.Error as AWS
import Data.Text (Text)
import qualified Index
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  AWS.env >>= either AWS.failOnInit runLambdaFunctions

runLambdaFunctions :: Startup.Env -> IO ()
runLambdaFunctions env =
  either AWS.failOnInit (AWS.lambdaLoop env)
  $ resolveLambda (Startup.handler env)

resolveLambda :: Text -> Either Startup.Error (AWS.Lambda IO)
resolveLambda = \case
    "Index.handler" -> pure Index.lambda
    handler -> Left $ Startup.HandlerNotFound handler
