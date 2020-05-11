{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

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

module Club
  ( Club,
    clubApi,
    club,
  )
where

import About (About (..))
import AllPosts
import Control.Monad.Except (throwError)
import qualified Data.List as List
import Data.Post (Post (postMeta), PostMeta (postMetaSlug))
import Data.Text (Text)
import Lucid
import Post
import Servant
  ( (:<|>) (..),
    (:>) (..),
    Capture,
    Get,
    Proxy (..),
    Raw,
    Server,
    err404,
    serveDirectoryWebApp,
  )
import Servant.Atom
import Servant.HTML.Lucid (HTML)
import Servant.RSS
import qualified Text.Atom.Feed as Atom
import Text.Atom.Xmlbf ()
import qualified Text.RSS.Syntax as RSS

type Club =
  Get '[HTML] About
    :<|> "posts" :> Get '[HTML] AllPosts
    :<|> "posts" :> "atom.xml" :> Get '[Atom] Atom.Feed
    -- :<|> "posts" :> "rss.xml" :> Get '[RSS] RSS.RSS
    :<|> "post" :> Capture "slug" Text :> Get '[HTML] Post
    :<|> Raw

clubApi :: Proxy Club
clubApi = Proxy

club :: String -> Server Club
club staticDir =
  pure About
    :<|> pure allPosts
    :<|> pure (toAtomFeed allPosts)
    :<|> ( \slug -> do
             let bySlug = (slug ==) . postMetaSlug . postMeta
             maybe (throwError err404) pure (lookupPost bySlug allPosts)
         )
    :<|> serveDirectoryWebApp staticDir
  where
    allPosts = AllPosts (fmap snd $(embedPosts "posts"))
