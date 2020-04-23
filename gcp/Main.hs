{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import System.Environment

type Club = Get '[HTML] ()

instance ToHtml () where
  toHtml _ =
    html_
      $ body_
      $ h1_ "hello"

club :: Server Club
club = pure ()

main :: IO ()
main = do
  port <- read @Int <$> getEnv "PORT"
  run port $ serve @Club Proxy club
