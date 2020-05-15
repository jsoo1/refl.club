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
    toAtomFeed,
    embedPosts,
    lookupPost,
  )
where

import qualified Club.Html as Club
import Data.Foldable (maximumBy, traverse_)
import Data.Function (on)
import qualified Data.List as List
import Data.Post (Post (..), PostMeta (..))
import qualified Data.Post as Post
import Data.Post.TH (embedPosts)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime (zonedTimeToUTC)
import Lucid
import qualified Post
import qualified Text.Atom.Feed as Atom
import qualified Text.RSS.Syntax as RSS
import qualified Text.URI as URI

newtype AllPosts = AllPosts [Post]

lookupPost :: (Post -> Bool) -> AllPosts -> Maybe Post
lookupPost f (AllPosts ps) = List.find f ps

instance ToHtml AllPosts where

  toHtmlRaw = toHtml

  toHtml (AllPosts ps) =
    doctypehtml_ $ do
      head_ $ do
        title_ "Posts - John Soo"
        Club.css
      body_ $ do
        Club.navBar $ Just Club.NavLocationPosts
        section_ [id_ "main"] $ ul_ $ do
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

toAtomFeed :: AllPosts -> Atom.Feed
toAtomFeed (AllPosts ps) = Atom.Feed
  { Atom.feedId = "https://www.refl.club/posts",
    Atom.feedTitle = Atom.TextString "John Soo",
    Atom.feedUpdated = T.pack $ iso8601Show $ postMetaDate $ latestEntry ps,
    Atom.feedAuthors = Post.atomAuthor . postMeta <$> ps,
    Atom.feedCategories = mempty,
    Atom.feedContributors = mempty,
    Atom.feedGenerator = Nothing,
    Atom.feedIcon = mempty,
    Atom.feedLinks = pure $ Atom.Link
      { Atom.linkHref = "https://www.refl.club/posts/atom.xml",
        Atom.linkRel = Just $ Left "self",
        Atom.linkType = Nothing,
        Atom.linkHrefLang = Nothing,
        Atom.linkTitle = Nothing,
        Atom.linkLength = Nothing,
        Atom.linkAttrs = mempty,
        Atom.linkOther = mempty
      },
    Atom.feedLogo = mempty,
    Atom.feedRights = Nothing,
    Atom.feedSubtitle = Nothing,
    Atom.feedEntries = Post.atomEntry <$> ps,
    Atom.feedAttrs = mempty,
    Atom.feedOther = mempty
  }
  where
    latestEntry =
      maximumBy (compare `on` (zonedTimeToUTC . postMetaDate)) . fmap postMeta
