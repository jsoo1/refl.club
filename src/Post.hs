{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Post where

import qualified Club.Html as Club
import Data.Org (Language (..), OrgDoc (..))
import Data.Org.Lucid (Highlighting, OrgStyle (..))
import qualified Data.Org.Lucid as Org
import Data.Post (Post (..), PostMeta (..))
import qualified Data.Post as Post
import Data.Text (Text)
import qualified Data.Time.Format as Time
import Lucid

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
        section_ $ do
          h1_ $ toHtml $ postMetaTitle postMeta
          byLine postMeta
          toHtml $ Org.body orgStyle (Post.postToOrg p)

byLine :: Monad m => PostMeta -> HtmlT m ()
byLine PostMeta {..} = do
  p_ [style_ "font-style:italic;"] $ toHtml postMetaDescription
  p_ [style_ "display:flex;", style_ "flex-wrap:wrap;"] $ do
    span_ $ toHtml $ Post.formatDate postMetaDate
    Club.verticalSep ""
    span_ $ toHtml postMetaAuthor
    Club.verticalSep ""
    a_
      [href_ ("mailto:" <> postMetaEmail)]
      (toHtml postMetaEmail)
