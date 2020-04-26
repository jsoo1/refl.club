{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Post where

import qualified Club.Html as Club
import Data.Org (OrgDoc(..))
import Data.Org.Lucid (OrgStyle(..))
import qualified Data.Org.Lucid as Org
import Data.Post (PostMeta(..), Post(..))
import qualified Data.Post as Post
import qualified Data.Time.Format as Time
import Lucid

orgStyle :: OrgStyle
orgStyle =
  Org.defaultStyle
    { includeTitle = False
    , tableOfContents = Nothing
    , bootstrap = False
    , highlighting = Org.codeHTML
    , hrBetweenSections = False
    }

instance ToHtml Post where
  toHtmlRaw = toHtml

  toHtml p@Post {..} =
    doctypehtml_ $ do
      head_ $ do
        title_ $ toHtml $ postMetaTitle postMeta
        Club.css
      body_ $ section_ $ do
        h1_ $ toHtml $ postMetaTitle postMeta
        p_ $ toHtml $ Post.formatDate $ postMetaDate postMeta
        p_ "John Soo"
        toHtml $ Org.body orgStyle (Post.postToOrg p)

instance ToHtml OrgDoc where
  toHtmlRaw = toHtml

  toHtml OrgDoc {..} =
    p_ "hi"
