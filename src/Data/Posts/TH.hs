{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Posts.TH (embedPosts) where

import Control.Exception (ErrorCall(..), Exception(..), throw)
import Data.ByteString (ByteString)
import Data.Data (Typeable, Data, cast)
import Data.FileEmbed (embedDir, getDir, bsToExp)
import Data.Maybe (fromMaybe)
import Data.Org
       ( OrgFile(..),
         OrgDoc(..),
         Section(..),
         Words(..),
         URL(..),
         Block(..),
         Language(..),
         ListItems(..),
         Item(..),
         Row(..),
         Column(..),
         org)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding.Error (UnicodeException)
import Data.List.NonEmpty (NonEmpty(..))
import Language.Haskell.TH.Syntax
       (Exp (..), Lift, Q, dataToExpQ, lift, runIO, Quasi(qAddDependentFile))

embedPosts :: FilePath -> Q Exp
embedPosts fp = do
    typ <- [t| [OrgFile] |]
    orgFiles <- runIO (postsDir fp) >>= either (fail . show) pure
    e <- ListE <$> traverse lift orgFiles
    pure $ SigE e typ

postsDir :: FilePath -> IO (Either DecodeError [OrgFile])
postsDir fp = do
  files <- getDir fp
  pure $ traverse (decodePost . snd) files

decodePost :: ByteString -> Either DecodeError OrgFile
decodePost bs = do
  txt <- either (Left . UnicodeError) pure $ decodeUtf8' bs
  maybe (Left ImproperOrgFile) pure $ org txt

data DecodeError = UnicodeError UnicodeException | ImproperOrgFile
  deriving (Exception)

instance Show DecodeError where
  show = \case
    UnicodeError e -> show e
    ImproperOrgFile -> "Malformed Org file"

instance Lift OrgFile where
  lift = liftDataWithText

deriving instance Data OrgFile
deriving instance Data OrgDoc
deriving instance Lift OrgDoc
deriving instance Data Section
deriving instance Lift Section
deriving instance Data Words
deriving instance Lift Words
deriving instance Data URL
deriving instance Lift URL
deriving instance Data Block
deriving instance Lift Block
deriving instance Data Language
deriving instance Lift Language
deriving instance Data ListItems
deriving instance Lift ListItems
deriving instance Data Item
deriving instance Lift Item
deriving instance Data Row
deriving instance Lift Row
deriving instance Data Column
deriving instance Lift Column
deriving instance Lift a => Lift (NonEmpty a)

instance Lift T.Text where
  lift = liftText

-- | https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (fmap liftText . cast)
