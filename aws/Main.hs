{-# LANGUAGE LambdaCase #-}

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

module Main where

import qualified AWS
import qualified Data.AWS.Error as AWS
import Data.AWS.Startup as Startup
import Data.Text (Text)
import qualified Index
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  AWS.env >>= either AWS.failOnInit runLambdaFunctions

runLambdaFunctions :: Startup.Env -> IO ()
runLambdaFunctions env =
  either AWS.failOnInit (AWS.lambdaLoop env) $
    resolveLambda (Startup.handler env)

resolveLambda :: Text -> Either Startup.Error (AWS.Lambda IO)
resolveLambda = \case
  "Index.handler" -> pure Index.lambda
  handler -> Left $ Startup.HandlerNotFound handler
