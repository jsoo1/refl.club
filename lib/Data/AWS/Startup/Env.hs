{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.AWS.Startup.Env
  ( Env (..),
    RuntimeApi(..),
    Error (..),
    env,
  )
where

import Control.Error.Util (note)
import qualified Data.AWS.Error as AWS
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Natural (Natural)
import System.Posix.Env (getEnv)

-- | The startup environment variables parsed during bootstrap initialization.
--    Create using @startupEnv.
data Env
  = Env
      { handler :: Text,
        taskRoot :: Text,
        runtimeApi :: RuntimeApi
      }

data RuntimeApi = RuntimeApi
  { runtimeApiHost :: Text,
    runtimeApiPort :: Natural
  }

data Error
  = VarNotFound Text
  | InvalidRuntimeApi Text
  | HandlerNotFound Text
  | forall e. AWS.ToError e => FunctionInitError e

instance AWS.ToError Error where
  toError = \case 
    VarNotFound var -> AWS.Error
      { AWS.errorType = "VarNotFound",
        AWS.errorMessage = var,
        AWS.stackTrace = []
      }
    HandlerNotFound handler -> AWS.Error
      { AWS.errorType = "HandlerNotFound",
        AWS.errorMessage = handler,
        AWS.stackTrace = []
      }
    FunctionInitError e -> AWS.toError e

env :: IO (Either Error Env)
env =
  do
    h <- getEnv' handlerVar
    t <- getEnv' taskRootVar
    r <- fmap (T.splitOn ":") <$> getEnv' runtimeApiVar
    pure $ Env <$> h <*> t <*> apiUrl r
  where
    getEnv' var = note (VarNotFound (T.pack var)) . fmap T.pack <$> getEnv var
    apiUrl ts = do
      items <- ts
      let err = InvalidRuntimeApi $ T.intercalate ":" items
      case items of
        [host, port] -> RuntimeApi host <$> p
          where p :: Either Error Natural
                p = note err $ decodeStrict' $ T.encodeUtf8 port
        _ -> Left err
    handlerVar = "_HANDLER"
    taskRootVar = "LAMBDA_TASK_ROOT"
    runtimeApiVar = "AWS_LAMBDA_RUNTIME_API"
