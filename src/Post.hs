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

module Post where

import qualified Club.Html as Club
import Data.Org (Language (..), OrgDoc (..))
import Data.Org.Lucid (Highlighting, OrgStyle (..))
import qualified Data.Org.Lucid as Org
import Data.Post (Post (..), PostMeta (..))
import qualified Data.Post as Post
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time.Format as Time
import Data.Time.Format.ISO8601 (iso8601Show)
import Lucid
import qualified Text.Atom.Feed as Atom
import qualified Text.URI as URI

orgStyle :: OrgStyle
orgStyle =
  Org.defaultStyle
    { includeTitle = False,
      tableOfContents = Nothing,
      bootstrap = False,
      highlighting = prismHighlighting,
      hrBetweenSections = False
    }

languageClass :: Language -> Text
languageClass (Language l) = "language-" <> l

prismHighlighting :: Highlighting
prismHighlighting lang =
  pre_ . code_ [class_ (maybe "" languageClass lang)] . toHtml

instance ToHtml Post where

  toHtmlRaw = toHtml

  toHtml p@Post {..} =
    doctypehtml_ $ do
      head_ $ do
        title_ $ toHtml $ postMetaTitle postMeta
        Club.cmuSerif
        Club.css
        Club.prismCss
      body_ $ do
        Club.prismJs
        Club.navBar Nothing
        article_ [id_ "main"] $ do
          h1_ $ toHtml $ postMetaTitle postMeta
          byLine postMeta
          toHtml $ Org.body orgStyle (Post.postToOrg p)
          footer_ Club.ccBySa

byLine :: Monad m => PostMeta -> HtmlT m ()
byLine PostMeta {..} = do
  p_ [style_ "font-style:italic;"] $ toHtml postMetaDescription
  p_ [style_ "display:flex;", style_ "flex-wrap:wrap;"] $ do
    span_ $ toHtml $ Post.formatDate postMetaDate
    Club.verticalSep
    span_ $ toHtml postMetaAuthor
    Club.verticalSep
    a_
      [href_ ("mailto:" <> postMetaEmail)]
      (toHtml postMetaEmail)

atomAuthor :: PostMeta -> Atom.Person
atomAuthor PostMeta {..} = Atom.Person
  { Atom.personName = postMetaAuthor,
    Atom.personURI = URI.render <$> postMetaURI,
    Atom.personEmail = pure postMetaEmail,
    Atom.personOther = mempty
  }

atomEntry :: Post -> Atom.Entry
atomEntry post@Post {..} = Atom.Entry
  { Atom.entryId = "https://www.refl.club/post/" <> postMetaSlug postMeta,
    Atom.entryTitle = Atom.TextString $ postMetaTitle postMeta,
    Atom.entryPublished = Just $ T.pack $ iso8601Show $ postMetaDate postMeta,
    Atom.entryUpdated = T.pack $ iso8601Show $ postMetaDate postMeta,
    Atom.entryAuthors = pure $ atomAuthor postMeta,
    Atom.entryCategories = mempty,
    Atom.entryContent =
      Just $ Atom.HTMLContent
        $ TL.toStrict
        $ Lucid.renderText
        $ toHtml
        $ Org.body orgStyle
        $ Post.postToOrg post,
    Atom.entryContributor = mempty,
    Atom.entryRights =
      Just $ Atom.HTMLString
        $ TL.toStrict
        $ Lucid.renderText Club.ccBySa,
    Atom.entryLinks = mempty,
    Atom.entrySummary = Nothing,
    Atom.entrySource = Nothing,
    Atom.entryInReplyTo = Nothing,
    Atom.entryInReplyTotal = Nothing,
    Atom.entryAttrs = mempty,
    Atom.entryOther = mempty
  }
