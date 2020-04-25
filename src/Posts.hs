{-# LANGUAGE TemplateHaskell #-}

module Posts where

import TH.Posts (embedPosts)
import Data.Org (OrgFile)

posts' :: [OrgFile]
posts' =
  $(embedPosts "posts")
