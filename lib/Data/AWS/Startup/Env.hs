module Data.AWS.Startup.Env
  ( Env (..),
    RuntimeApiHost,
    unRuntimeApiHost,
    Error,
    env,
  )
where

import Control.Error.Util (note)
import qualified Data.AWS.Error as AWS
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import System.Posix.Env (getEnv)

-- | The startup environment variables parsed during bootstrap initialization.
--    Create using @startupEnv.
data Env
  = Env
      { handler :: Text,
        taskRoot :: Text,
        runtimeApi :: RuntimeApiHost
      }

newtype RuntimeApiHost = RuntimeApiHost {unRuntimeApiHost :: Text}

newtype Error
  = VarNotFound Text
  deriving (Show)

instance AWS.ToError Error where
  toAWSError (VarNotFound var) = AWS.Error
    { AWS.errorType = "VarNotFound",
      AWS.errorMessage = var,
      AWS.stackTrace = []
    }

env :: IO (Either Error Env)
env =
  do
    h <- getEnv' handlerVar
    t <- getEnv' taskRootVar
    r <- fmap RuntimeApiHost <$> getEnv' runtimeApiVar
    pure $ Env <$> h <*> t <*> r
  where
    getEnv' var = note (VarNotFound (T.pack var)) . fmap T.pack <$> getEnv var
    handlerVar = "_HANDLER"
    taskRootVar = "LAMBDA_TASK_ROOT"
    runtimeApiVar = "AWS_LAMBDA_RUNTIME_API"
