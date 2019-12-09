{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Index where

import AWS.Lambda
import qualified Data.AWS.Error as AWS
import Data.AWS.Runtime
import qualified Data.AWS.Runtime.Response as Response
import qualified Data.AWS.Startup as Startup
import Data.Aeson
import GHC.Generics
import Lucid

data Person
  = Person {personName :: String, personAge :: Int}
  deriving (Generic, FromJSON, ToJSON)

handler' :: Person -> IO (Either String Person)
handler' person =
  if personAge person > 0
    then return (Right person)
    else return (Left "A person's age must be positive")

lambda :: Monad m => Lambda m
lambda = Lambda
  { lambdaSetup = setup 
  , lambdaHandler = handler
  }

setup :: Monad m => Startup.Env -> m (Either Startup.Error ())
setup Startup.Env {..} =
  pure $ Right ()

instance ToText () where
  toText = const ""

handler :: Monad m => MonadLambda () Startup.Error m Response
handler = pure $ Response ()
