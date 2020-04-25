{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Index where

import AWS.Lambda
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson
import qualified Data.AWS.Error as AWS
import Data.AWS.Runtime
import qualified Data.AWS.Runtime.Response as Response
import qualified Data.AWS.Startup as Startup
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Lucid

type MonadIndex = MonadLambda () Startup.Error IO Response

lambda :: Lambda IO
lambda = Lambda
  { lambdaSetup = setup 
  , lambdaHandler = handler
  }

setup :: Startup.Env -> IO (Either Startup.Error ())
setup Startup.Env {..} = pure $ Right ()

data ReflHtml = forall a. ReflHtml (Html a)

instance ToText ReflHtml where
  toText (ReflHtml x) = TL.toStrict $ renderText x

handler :: MonadIndex
handler = do
  ctx <- ask
  liftIO $ BLC.putStrLn $ encode ctx
  pure $ Response $ ReflHtml $ h1_ "hello"
