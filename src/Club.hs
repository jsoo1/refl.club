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
    api,
    server,
  )
where

import About (About (..))
import AllPosts
import Club.Git (gitHead, gitHeadSym)
import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import qualified Data.List as List
import Data.Post (Post (postMeta), PostMeta (postMetaSlug))
import Data.Post.TH (embedPosts, postsApi, postsDirQ)
import Data.Text (Text)
import qualified Data.Text as Text
import Embed.Text (embedTextFile)
import GHC.TypeLits (AppendSymbol, Symbol)
import Lucid
import Post
import Servant
  ( Context (EmptyContext),
    Get,
    PlainText,
    Proxy (..),
    Server,
    err404,
    (:<|>) (..),
    (:>) (..),
  )
import Servant.Atom
import Servant.CSS (CSS)
import Servant.HTML.Lucid (HTML)
import Servant.PDF (PDF)
import Servant.PgpKey (PgpKey)
import Servant.RSS
import Servant.Seo (Disallow, Frequency, Period (..), Priority)
import Servant.Seo.Orphans ()
import Servant.Woff
import qualified Text.Atom.Feed as Atom
import Text.Atom.Xmlbf ()
import qualified Text.RSS.Syntax as RSS

type Club =
  Priority '(0, 9) :> Frequency 'Monthly :> Get '[HTML] About
    :<|> Frequency 'Monthly :> "john-soo.asc" :> Get '[PgpKey] Text
    :<|> Frequency 'Monthly :> AppendSymbol $(gitHeadSym) "-john-soo-resume.pdf" :> Get '[PDF] ByteString
    :<|> Priority '(1, 0) :> Frequency 'Monthly :> "posts" :> Get '[HTML] AllPosts
    :<|> $(fmap fst (postsDirQ "posts" >>= postsApi))
    :<|> Disallow (AppendSymbol $(gitHeadSym) "-refl.css") :> Get '[CSS] Text
    :<|> Disallow "iosevka-regular.woff" :> Get '[Woff] ByteString

api :: Proxy Club
api = Proxy

allPosts :: AllPosts
allPosts = AllPosts (fmap snd $(embedPosts "posts"))

server :: Server Club
server =
  pure About
    :<|> pure $(embedTextFile "john-soo.asc")
    :<|> pure $(embedFile "john-soo-resume.pdf")
    :<|> pure allPosts
    :<|> $(fmap snd (postsDirQ "posts" >>= postsApi))
    :<|> pure $(embedTextFile "refl.css")
    :<|> pure $(embedFile "iosevka-regular.woff")
