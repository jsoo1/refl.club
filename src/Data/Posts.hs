{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Posts (Post(..), PostError(..)) where

import Data.Org (OrgFile(..), OrgDoc(..))
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UTCTime)

data PostError = NoTitle | NoSlug | NoDate | MalformedDate Text
data Post = Post { postMeta :: PostMeta, postDoc :: OrgDoc }
data PostMeta = PostMeta
  { postMetaSlug :: Text
  , postMetaTitle :: Text
  , postMetaDate :: UTCTime
  }

orgToPost :: OrgFile -> Either PostError Post
orgToPost OrgFile {..} = do
  postMetaTitle <- maybe (Left NoTitle) pure $ Map.lookup "title" orgMeta
  postMetaSlug <- maybe (Left NoSlug) pure $ Map.lookup "slug" orgMeta
  postMetaDate <- maybe (Left NoDate) pure $ Map.lookup "date" orgMeta
  pure Post { postMeta = PostMeta {..}, postDoc = orgDoc }
