{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AWS.Lambda where

import Data.AWS.Runtime
import qualified Data.AWS.Startup as Startup

data Lambda m
  = forall a.
    Lambda
      { lambdaRunSetup :: Startup.Env -> m a,
        lambdaRunHandler :: Context a -> a
      }
