{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Post where

import qualified Club.Html as Club
import Data.Org (OrgDoc(..))
import Data.Post (PostMeta(..), Post(..))
import qualified Data.Post as Post
import qualified Data.Time.Format as Time
import Lucid

formatDate :: Time.FormatTime t => t -> String
formatDate = Time.formatTime Time.defaultTimeLocale Post.dateFormat

instance ToHtml Post where
  toHtmlRaw = toHtml

  toHtml Post {..} =
    doctypehtml_ $ do
      head_ $ do
        title_ $ toHtml $ postMetaTitle postMeta
        Club.css
      body_ $ section_ $ do
        h1_ $ toHtml $ postMetaTitle postMeta
        p_ $ toHtml $ formatDate $ postMetaDate postMeta
        p_ "John Soo"
        toHtml postDoc

instance ToHtml OrgDoc where
  toHtmlRaw = toHtml

  toHtml OrgDoc {..} =
    p_ "hi"
