module Data.AWS.Runtime.Context where

import qualified Data.AWS.Startup as Startup

data Context a
  = Context
      { contextStartupEnv :: Startup.Env,
        contextStartupSetup :: a
      }
