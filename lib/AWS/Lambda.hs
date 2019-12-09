{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
 
module AWS.Lambda where

import qualified Data.AWS.Error as AWS
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Except (ExceptT, MonadError)
import Data.AWS.Runtime
import qualified Data.AWS.Startup as Startup

type MonadLambda ctx e m a = ReaderT (Context ctx) (ExceptT e m) a

data Lambda m
  = forall startupError runtimeError ctx.
    (AWS.ToError startupError, AWS.ToError runtimeError) =>
    Lambda
      { lambdaSetup :: Startup.Env -> m (Either startupError ctx),
        lambdaHandler :: MonadLambda ctx runtimeError m Data.AWS.Runtime.Response
      }
