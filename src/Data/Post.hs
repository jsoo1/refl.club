{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

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
    mostRecentUpdateTime,
    orgToPost,
    postToOrg,
  )
where

import Control.Monad.Catch (Exception, SomeException, catch)
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Org (OrgDoc (..), OrgFile (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (ZonedTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import qualified Data.Time.Format as Time
import Language.Haskell.TH.Syntax (Lift (..))
import qualified Language.Haskell.TH.Syntax as TH
import Text.URI (URI)
import qualified Text.URI as URI

data PostError
  = NoAuthor
  | MalformedURI SomeException
  | NoEmail
  | NoTitle
  | NoSlug
  | NoPublished
  | NoDescription
  | MalformedDate String
  deriving (Exception)

data Post = Post {postMeta :: PostMeta, postDoc :: OrgDoc}
  deriving (Show, Lift)

data PostMeta
  = PostMeta
      { postMetaAuthor :: Text,
        postMetaURI :: Maybe URI,
        postMetaEmail :: Text,
        postMetaSlug :: Text,
        postMetaTitle :: Text,
        postMetaDescription :: Text,
        postMetaPublished :: ZonedTime,
        postMetaUpdated :: [ZonedTime]
      }
  deriving (Show, Data, Lift)

instance Lift ZonedTime where
  lift = TH.liftData

dateFormat :: String
dateFormat = "%Y-%m-%d %I:%M%p %Z"

formatDate :: Time.FormatTime t => t -> String
formatDate = Time.formatTime Time.defaultTimeLocale "%Y-%02m-%02d %I:%M%p %Z"

orgToPost :: OrgFile -> Either PostError Post
orgToPost OrgFile {..} = do
  postMetaAuthor <- maybe (Left NoAuthor) pure $ Map.lookup "author" orgMeta
  postMetaURI' <- pure $ Map.lookup "uri" orgMeta
  postMetaURI <- first MalformedURI $ traverse URI.mkURI postMetaURI'
  postMetaEmail <- maybe (Left NoEmail) pure $ Map.lookup "email" orgMeta
  postMetaTitle <- maybe (Left NoTitle) pure $ Map.lookup "title" orgMeta
  postMetaSlug <- maybe (Left NoSlug) pure $ Map.lookup "slug" orgMeta
  postMetaPublished' <- maybe (Left NoPublished) (pure . T.unpack) $ Map.lookup "published" orgMeta
  postMetaPublished <- parseTime postMetaPublished'
  postMetaUpdated' <- maybe (pure []) (pure . fmap T.unpack . T.splitOn ",") $ Map.lookup "updated" orgMeta
  postMetaUpdated <- traverse parseTime postMetaUpdated'
  postMetaDescription <- maybe (Left NoDescription) pure $ Map.lookup "description" orgMeta
  pure Post {postMeta = PostMeta {..}, postDoc = orgDoc}
  where
    parseTime s = maybe (Left (MalformedDate s)) Right
      $ Time.parseTimeM True Time.defaultTimeLocale dateFormat s

postToOrg :: Post -> OrgFile
postToOrg Post {..} =
  OrgFile meta postDoc
  where
    meta :: Map.Map Text Text
    meta = Map.fromList $
        [ ("title", postMetaTitle postMeta)
        , ("slug", postMetaSlug postMeta)
        , ("published", T.pack (formatDate (postMetaPublished postMeta)))
        , ("description", postMetaDescription postMeta)
        ] <> if null (postMetaUpdated postMeta)
               then []
               else let formatted = fmap formatDate (postMetaUpdated postMeta) in
                 [("updated", T.pack (intercalate "," formatted))]

instance Show PostError where
  show = \case
    NoAuthor -> "Missing an #+author:"
    MalformedURI e -> "URI is malformed: " <> show e
    NoEmail -> "Missing an #+email:"
    NoTitle -> "Missing a #+title:"
    NoSlug -> "Missing a #+slug:"
    NoPublished -> "Missing a #+published:"
    NoDescription -> "Missing a #+description:"
    MalformedDate d ->
      "Malformed date " <> d <> " (use date format: " <> dateFormat <> ")"


mostRecentUpdateTime :: PostMeta -> ZonedTime
mostRecentUpdateTime PostMeta{..} =
  maximumBy (compare `on` zonedTimeToUTC)
    (postMetaPublished :| postMetaUpdated)
