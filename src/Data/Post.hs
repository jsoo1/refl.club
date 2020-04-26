{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Data.Post
  ( Post(..)
  , PostMeta(..)
  , PostError(..)
  , dateFormat
  , formatDate
  , orgToPost
  , postToOrg
  ) where

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

data PostError
  = NoTitle
  | NoSlug
  | NoDate
  | NoDescription
  | MalformedDate String
data Post = Post { postMeta :: PostMeta, postDoc :: OrgDoc }
  deriving (Show, Lift)
data PostMeta = PostMeta
  { postMetaSlug :: Text
  , postMetaTitle :: Text
  , postMetaDescription :: Text
  , postMetaDate :: UTCTime
  } deriving (Show, Data)

instance Lift PostMeta where
  lift = T.liftDataWithText

dateFormat :: String
dateFormat = "%Y-%-m-%-d"

formatDate :: Time.FormatTime t => t -> String
formatDate = Time.formatTime Time.defaultTimeLocale dateFormat

orgToPost :: OrgFile -> Either PostError Post
orgToPost OrgFile {..} = do
  postMetaTitle <- maybe (Left NoTitle) pure $ Map.lookup "title" orgMeta
  postMetaSlug <- maybe (Left NoSlug) pure $ Map.lookup "slug" orgMeta
  postMetaDate' <- maybe (Left NoDate) (pure . T.unpack) $ Map.lookup "date" orgMeta
  postMetaDate <- maybe (Left (MalformedDate postMetaDate')) pure $ parseTime postMetaDate'
  postMetaDescription <- maybe (Left NoDescription) pure $  Map.lookup "description" orgMeta
  pure Post { postMeta = PostMeta {..}, postDoc = orgDoc }
  where
    parseTime = Time.parseTimeM True Time.defaultTimeLocale dateFormat

postToOrg :: Post -> OrgFile
postToOrg Post {..} =
  OrgFile meta postDoc
  where
    meta :: Map.Map Text Text
    meta =
      Map.insert "title" (postMetaTitle postMeta)
      $ Map.insert "slug" (postMetaSlug postMeta)
      $ Map.insert "date" (T.pack (formatDate (postMetaDate postMeta)))
      $ Map.insert "description" (postMetaDescription postMeta) mempty

instance Show PostError where
  show = \case
    NoTitle -> "Missing a #+title:"
    NoSlug -> "Missing a #+slug:"
    NoDate -> "Missing a #+date:"
    NoDescription -> "Missing a #+description:"
    MalformedDate d ->
      "Date " <> d <> "is malformed, please use format " <> dateFormat
