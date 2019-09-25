module Main where

import Aws.Lambda

import qualified Lib

main :: IO ()
main = do
  name <- getHandlerName
  ctx <- getContext
  input <- getInput
  case name of
    "src/Lib.handler" -> do
      result <- Lib.handler input ctx
      publish result
    _ -> fail
