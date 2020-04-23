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
        title_ "John Soo"
        link_
          [ href_ "/static/refl.css",
            rel_ "stylesheet",
            type_ "text/css"
          ]
      body_ $ section_ $ do
        h1_ "John Soo"
        p_
          "Software Engineer, \
          \functional programming enthusiast, \
          \aspiring proof engineer."
        p_ "5A15 8FAF 406A 748A 81A9  DC4E 4F43 7A76 B448 A23B"
        nav_ $ ul_ [style_ "display:flex;"] $ do
          a_ [href_ "https://github.com/jsoo1"] $ li_ "github.com/jsoo1"
          a_ [href_ "https://twitter.com/jsoo1"] $ li_ "twitter.com/jsoo1"

club :: String -> Server Club
club staticDir =
  pure () :<|> serveDirectoryWebApp staticDir

main :: IO ()
main = do
  staticDir <- head <$> getArgs
  port <- read @Int <$> getEnv "PORT"
  run port $ serve @Club Proxy $ club staticDir
