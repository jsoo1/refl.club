{-# LANGUAGE TemplateHaskell #-}

module Club.Git
  ( gitHead,
    gitHeadSym,
  )
where

import Data.ByteString as BS
import Data.Char (isSpace)
import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.IO as Text
import Data.Text.Lift (liftText)
import Language.Haskell.TH.Syntax
  ( Exp (..),
    Lift,
    Q,
    Quasi (qAddDependentFile),
    TyLit (..),
    Type (..),
    lift,
    runIO,
  )

gitHead :: Q Exp
gitHead = do
  qAddDependentFile ".git/HEAD"
  typ <- [t|Text|]
  e <- liftText =<< runIO readHead
  pure $ SigE e typ

readHead :: IO Text
readHead = do
  txt <- Text.readFile ".git/HEAD"
  ref <- case Text.words txt of
    [ref] -> pure ref
    [ref, file] -> Text.readFile (".git/" <> Text.unpack file)
    _ -> fail $ "unexpected .git/HEAD: " <> Text.unpack txt
  pure (Text.filter (not . isSpace) ref)

gitHeadSym :: Q Type
gitHeadSym = do
  qAddDependentFile ".git/HEAD"
  head <- runIO readHead
  pure $ LitT $ StrTyLit $ Text.unpack head
