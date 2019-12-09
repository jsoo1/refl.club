module Data.AWS.Runtime.Context where

import Data.AWS.Runtime.Invocation (Event)
import qualified Data.AWS.Startup as Startup
import Hreq.Client

data Context a
  = Context
      { contextAWSEnv :: Startup.Env,
        contextEventHeaders :: [Header],
        contextEvent :: Event,
        contextEnv :: a
      }
