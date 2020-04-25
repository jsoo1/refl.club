{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Club (Club, club, clubApi)
import Network.Wai.Handler.Warp
import Servant (serve)
import System.Environment

main :: IO ()
main = do
  staticDir <- head <$> getArgs
  port <- read @Int <$> getEnv "PORT"
  run port $ serve clubApi $ club staticDir
