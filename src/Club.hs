{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Club (Club, clubApi, club) where

import Lucid
import About (About(..))
import Data.Text (Text)
import Servant ((:<|>)(..), Proxy(..), Get, Raw, Server, serveDirectoryWebApp)
import Servant.HTML.Lucid (HTML)

type Club =
  Get '[HTML] About :<|> Raw

clubApi :: Proxy Club
clubApi = Proxy

club :: String -> Server Club
club staticDir =
  pure About :<|> serveDirectoryWebApp staticDir
