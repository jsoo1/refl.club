{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
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
    postsApi,
    postsDirQ,
  )
where

import qualified AllPosts
import Control.Exception (ErrorCall (..), Exception (..), throw)
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable, cast)
import Data.FileEmbed (getDir)
import Data.Foldable (foldl', traverse_)
import Data.Maybe (fromMaybe)
import Data.Org (org)
import Data.Post (Post (postMeta), PostError, PostMeta (postMetaSlug), orgToPost)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import GHC.TypeLits (KnownSymbol)
import Language.Haskell.TH.Syntax
  ( Exp (..),
    Lift,
    Q,
    Quasi (qAddDependentFile),
    TyLit (StrTyLit),
    Type (..),
    lift,
    newName,
    runIO,
  )
import Servant
  ( Get,
    Server,
    (:<|>) (..),
    (:>) (..),
  )
import Servant.Atom
import Servant.HTML.Lucid (HTML)
import Servant.Seo (Frequency, Period (..), Priority)
import qualified Text.Atom.Feed as Atom

postsApi :: [(FilePath, Post)] -> Q (Type, Exp)
postsApi posts = do
  atomFeedRoute <- atomFeedRouteQ
  atomFeedServer <- atomFeedServerQ posts
  let atomFeed = pure (atomFeedRoute, atomFeedServer)
  foldl' postRoute atomFeed posts

postRoute :: Q (Type, Exp) -> (FilePath, Post) -> Q (Type, Exp)
postRoute q (_, post) = do
  (api, server) <- q
  typ <- [t|$(pure api) :<|> Frequency 'Monthly :> "post" :> $(slugSym post) :> Get '[HTML] Post|]
  exp <- [e|$(pure server) :<|> pure $(lift post)|]
  pure (typ, exp)
  where
    slugSym = pure . LitT . StrTyLit . T.unpack . postMetaSlug . postMeta

atomFeedRouteQ :: Q Type
atomFeedRouteQ =
  [t|"posts" :> "atom.xml" :> Frequency 'Monthly :> Get '[Atom] Atom.Feed|]

atomFeedServerQ :: [(FilePath, Post)] -> Q Exp
atomFeedServerQ posts =
  let allPosts = lift $ fmap snd posts
   in [e|pure (AllPosts.toAtomFeed $allPosts)|]

embedPosts :: FilePath -> Q Exp
embedPosts fp = do
  typ <- [t|[(FilePath, Post)]|]
  orgFiles <- postsDirQ fp
  e <- ListE <$> traverse lift orgFiles
  pure $ SigE e typ

postsDirQ :: FilePath -> Q [(FilePath, Post)]
postsDirQ fp = do
  posts' <- runIO (postsDir fp)
  posts <- either (fail . show) pure posts'
  traverse_ (qAddDependentFile . (\p -> fp <> "/" <> p) . fst) posts
  pure posts

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
