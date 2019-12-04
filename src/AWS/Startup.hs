module AWS.Startup
  ( StartupEnv (..),
    StartupError,
    startupEnv,
  )
where

import Data.AWS (ToAWSError (..), Error(errorType, errorMessage, stackTrace))
import qualified Data.AWS as AWS
import Control.Error.Util (note)
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import System.Posix.Env (getEnv)

data StartupEnv
  = StartupEnv
      { handler :: Text,
        taskRoot :: Text,
        runtimeApi :: Text
      }

newtype StartupError
  = VarNotFound Text
  deriving (Show)

instance ToAWSError StartupError where
  toAWSError (VarNotFound var) = AWS.Error
    { errorType = "VarNotFound",
      errorMessage = var,
      stackTrace = []
    }

startupEnv :: IO (Either StartupError StartupEnv)
startupEnv =
  do
    h <- getEnv' handlerVar
    t <- getEnv' taskRootVar
    r <- getEnv' runtimeApiVar
    pure $ StartupEnv <$> h <*> t <*> r
  where
    getEnv' var = note (VarNotFound (T.pack var)) . fmap T.pack <$> getEnv var
    handlerVar = "_HANDLER"
    taskRootVar = "LAMBDA_TASK_ROOT"
    runtimeApiVar = "AWS_LAMBDA_RUNTIME_API"
