{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Posts where

import qualified Club.Html as Club
import Data.Foldable (traverse_)
import Data.Post.TH (embedPosts)
import Data.Post (Post(..), PostMeta(..))
import Lucid

posts :: [Post]
posts = snd <$> $(embedPosts "posts")

newtype AllPosts = AllPosts [Post]

instance ToHtml AllPosts where
  toHtmlRaw = toHtml

  toHtml (AllPosts ps) =
    doctypehtml_ $ do
      head_ $ do
        title_ "All Posts - John Soo"
        Club.css
      body_ $ section_ $ do
        h1_ "All Posts"
        ul_ $ traverse_ (postLinkItem . postMeta) ps

postLinkItem :: Monad m => PostMeta -> HtmlT m ()
postLinkItem PostMeta {..} =
  li_ $ do
    a_ [href_ ("/post/" <> postMetaSlug)] (toHtml postMetaTitle)
    toHtml postMetaDescription
