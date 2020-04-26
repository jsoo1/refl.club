{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Data.Posts (Post(..), PostError(..), orgToPost) where

import Data.Data (Data)
import Data.Org (OrgFile(..), OrgDoc(..))
import Data.Org.Instances ()
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Lift as T
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.Time.Format as Time
import Language.Haskell.TH.Syntax (Lift(..))

data PostError = NoTitle | NoSlug | NoDate | MalformedDate String
data Post = Post { postMeta :: PostMeta, postDoc :: OrgDoc }
  deriving (Show, Lift)
data PostMeta = PostMeta
  { postMetaSlug :: Text
  , postMetaTitle :: Text
  , postMetaDate :: UTCTime
  } deriving (Show, Data)

instance Lift PostMeta where
  lift = T.liftDataWithText

dateFormat :: String
dateFormat = "%Y-%-m-%-d"

orgToPost :: OrgFile -> Either PostError Post
orgToPost OrgFile {..} = do
  postMetaTitle <- maybe (Left NoTitle) pure $ Map.lookup "title" orgMeta
  postMetaSlug <- maybe (Left NoSlug) pure $ Map.lookup "slug" orgMeta
  postMetaDate' <- maybe (Left NoDate) (pure . T.unpack) $ Map.lookup "date" orgMeta
  postMetaDate <- maybe (Left (MalformedDate postMetaDate')) pure $ parseTime postMetaDate'
  pure Post { postMeta = PostMeta {..}, postDoc = orgDoc }
  where
    parseTime = Time.parseTimeM True Time.defaultTimeLocale dateFormat

instance Show PostError where
  show = \case
    NoTitle -> "Missing a #+title:"
    NoSlug -> "Missing a #+slug:"
    NoDate -> "Missing a #+date:"
    MalformedDate d ->
      "Date " <> d <> "is malformed, please use format " <> dateFormat
