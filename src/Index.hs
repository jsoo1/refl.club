{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Index where

import AWS.Lambda
import Data.AWS.Runtime
import qualified Data.AWS.Startup as Startup
import Data.Aeson
import GHC.Generics
import Lucid

data Person
  = Person {personName :: String, personAge :: Int}
  deriving (Generic, FromJSON, ToJSON)

handler :: Person -> IO (Either String Person)
handler person =
  if personAge person > 0
    then return (Right person)
    else return (Left "A person's age must be positive")

lambda :: Monad m => Lambda m
lambda = Lambda
  { lambdaRunSetup = setup
  , lambdaRunHandler = contextStartupSetup
  }

setup :: Monad m => Startup.Env -> m ()
setup Startup.Env {..} =
  pure ()
