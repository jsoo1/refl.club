module Main where

import System.Environment

main :: IO ()
main = do
  port <- getEnv "PORT"
  print port
