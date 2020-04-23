{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import System.Environment

type Club =
  Get '[HTML] ()
    :<|> "static" :> Raw

instance ToHtml () where

  toHtmlRaw = toHtml

  toHtml _ =
    doctypehtml_ $ do
      head_ $ do
        title_ "Welcome to the Refl Club"
        link_
          [ href_ "/static/refl.css",
            rel_ "stylesheet",
            type_ "text/css"
          ]
      body_ $ section_ $ do
        h1_ "hello"
        p_ "have some text"

club :: String -> Server Club
club staticDir =
  pure () :<|> serveDirectoryWebApp staticDir

main :: IO ()
main = do
  staticDir <- head <$> getArgs
  port <- read @Int <$> getEnv "PORT"
  run port $ serve @Club Proxy $ club staticDir
