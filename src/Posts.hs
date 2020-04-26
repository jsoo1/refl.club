{-# LANGUAGE TemplateHaskell #-}

module Posts where

import Data.Posts.TH (embedPosts)
import Data.Posts (Post)

posts' :: [(FilePath, Post)]
posts' =
  $(embedPosts "posts")
