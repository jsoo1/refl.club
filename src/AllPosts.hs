{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
    a_ [href_ ("/post/" <> postMetaSlug)] (toHtml postMetaTitle)
    Club.verticalSep ""
    toHtml postMetaDescription
