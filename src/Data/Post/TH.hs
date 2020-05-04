{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Post.TH
  ( embedPosts,
  )
where

import Control.Exception (ErrorCall (..), Exception (..), throw)
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable, cast)
import Data.FileEmbed (bsToExp, embedDir, getDir)
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
