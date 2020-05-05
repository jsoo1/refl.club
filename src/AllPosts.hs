{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

module AllPosts
  ( AllPosts (..),
    embedPosts,
    lookupPost,
  )
where

import qualified Club.Html as Club
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.Post (Post (..), PostMeta (..))
import qualified Data.Post as Post
import Data.Post.TH (embedPosts)
import Data.Text (Text)
import Lucid

newtype AllPosts = AllPosts [Post]

lookupPost :: (Post -> Bool) -> AllPosts -> Maybe Post
lookupPost f (AllPosts ps) = List.find f ps

instance ToHtml AllPosts where

  toHtmlRaw = toHtml

  toHtml (AllPosts ps) =
    doctypehtml_ $ do
      head_ $ do
        title_ "Posts - John Soo"
        Club.cmuSerif
        Club.css
      body_ $ section_ $ ul_ $ do
        h1_ "Posts"
        traverse_ (postLinkItem . postMeta) ps

postLinkItem :: Monad m => PostMeta -> HtmlT m ()
postLinkItem PostMeta {..} =
  li_ [style_ "display:flex;"] $ do
    toHtml $ Post.formatDate postMetaDate
    Club.verticalSep
    a_ [href_ ("/post/" <> postMetaSlug)] (toHtml postMetaTitle)
    Club.verticalSep
    toHtml postMetaDescription
