{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Post.TH (embedPosts) where

import Control.Exception (ErrorCall(..), Exception(..), throw)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import Data.Data (Typeable, Data, cast)
import Data.FileEmbed (embedDir, getDir, bsToExp)
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
       ( Exp(..), Lift, Q, lift, runIO, Quasi(qAddDependentFile))

embedPosts :: FilePath -> Q Exp
embedPosts fp = do
    typ <- [t| [(FilePath, Post)] |]
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
  txt <- either (Left . UnicodeError) pure $ decodeUtf8' bs
  org <- maybe (Left ImproperOrgFile) pure $ org txt
  either (Left . ImproperPost) pure $ orgToPost org

data DecodeError
  = UnicodeError UnicodeException
  | ImproperOrgFile
  | ImproperPost PostError
  deriving (Exception)

instance Show DecodeError where
  show = \case
    UnicodeError e -> show e
    ImproperOrgFile -> "Malformed Org file"
