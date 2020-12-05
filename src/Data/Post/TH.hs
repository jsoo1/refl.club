{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Data.Post.TH
  ( embedPosts,
  )
where

import Control.Exception (ErrorCall (..), Exception (..), throw)
import Control.Monad ((<=<), void)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable, cast)
import Data.FileEmbed (getDir)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Org (org)
import Data.Org.Instances ()
import Data.Post (Post, PostError, orgToPost)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Data.Text.Lift (liftDataWithText)
import Language.Haskell.TH.Syntax
  ( Exp (..),
    Lift,
    Q,
    Quasi (qAddDependentFile),
    lift,
    runIO,
  )

embedPosts :: FilePath -> Q Exp
embedPosts fp = do
  void (qAddDependentFile fp)
  typ <- [t|[(FilePath, Post)]|]
  orgFiles <- either (fail . show) pure =<< runIO (postsDir fp)
  traverse_ (qAddDependentFile . (\p -> fp <> "/" <> p) . fst) orgFiles
  e <- ListE <$> traverse lift orgFiles
  pure $ SigE e typ

postsDir :: FilePath -> IO (Either DecodeError [(FilePath, Post)])
postsDir fp = do
  files <- getDir fp
  pure $ traverse (traverse decodePost) files

decodePost :: ByteString -> Either DecodeError Post
decodePost bs = do
  txt <- first UnicodeError $ decodeUtf8' bs
  org <- maybe (Left ImproperOrgFile) pure $ org txt
  first ImproperPost $ orgToPost org

data DecodeError
  = UnicodeError UnicodeException
  | ImproperOrgFile
  | ImproperPost PostError
  deriving (Exception)

instance Show DecodeError where
  show = \case
    UnicodeError e -> show e
    ImproperOrgFile -> "Malformed Org file"
    ImproperPost e -> show e
