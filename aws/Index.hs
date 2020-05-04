{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of refl.club - a personal website and blog
-- Copyright (C) 2020 John Soo <jsoo1@asu.edu>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Index where

import AWS.Lambda
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import qualified Data.AWS.Error as AWS
import Data.AWS.Runtime
import qualified Data.AWS.Runtime.Response as Response
import qualified Data.AWS.Startup as Startup
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Lucid

type MonadIndex = MonadLambda () Startup.Error IO Response

lambda :: Lambda IO
lambda = Lambda
  { lambdaSetup = setup,
    lambdaHandler = handler
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
