{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of refl.club - a personal website and blog
-- Copyright (C) 2020 John Soo <jsoo1@asu.edu>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Data.Post
  ( Post (..),
    PostMeta (..),
    PostError (..),
    dateFormat,
    formatDate,
    orgToPost,
    postToOrg,
  )
where

import Data.Data (Data)
import qualified Data.Map as Map
import Data.Org (OrgDoc (..), OrgFile (..))
import Data.Org.Instances ()
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lift as T
import Data.Time (UTCTime)
import qualified Data.Time.Format as Time
import Language.Haskell.TH.Syntax (Lift (..))

data PostError
  = NoAuthor
  | NoEmail
  | NoTitle
  | NoSlug
  | NoDate
  | NoDescription
  | MalformedDate String

data Post = Post {postMeta :: PostMeta, postDoc :: OrgDoc}
  deriving (Show, Lift)

data PostMeta
  = PostMeta
      { postMetaAuthor :: Text,
        postMetaEmail :: Text,
        postMetaSlug :: Text,
        postMetaTitle :: Text,
        postMetaDescription :: Text,
        postMetaDate :: UTCTime
      }
  deriving (Show, Data)

instance Lift PostMeta where
  lift = T.liftDataWithText

dateFormat :: String
dateFormat = "%Y-%m-%d"

formatDate :: Time.FormatTime t => t -> String
formatDate = Time.formatTime Time.defaultTimeLocale "%Y-%02m-%02d"

orgToPost :: OrgFile -> Either PostError Post
orgToPost OrgFile {..} = do
  postMetaAuthor <- maybe (Left NoAuthor) pure $ Map.lookup "author" orgMeta
  postMetaEmail <- maybe (Left NoEmail) pure $ Map.lookup "email" orgMeta
  postMetaTitle <- maybe (Left NoTitle) pure $ Map.lookup "title" orgMeta
  postMetaSlug <- maybe (Left NoSlug) pure $ Map.lookup "slug" orgMeta
  postMetaDate' <- maybe (Left NoDate) (pure . T.unpack) $ Map.lookup "date" orgMeta
  postMetaDate <- maybe (Left (MalformedDate postMetaDate')) pure $ parseTime postMetaDate'
  postMetaDescription <- maybe (Left NoDescription) pure $ Map.lookup "description" orgMeta
  pure Post {postMeta = PostMeta {..}, postDoc = orgDoc}
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
    NoAuthor -> "Missing an #+author:"
    NoEmail -> "Missing an #+email:"
    NoTitle -> "Missing a #+title:"
    NoSlug -> "Missing a #+slug:"
    NoDate -> "Missing a #+date:"
    NoDescription -> "Missing a #+description:"
    MalformedDate d ->
      "Date " <> d <> "is malformed, please use format " <> dateFormat
