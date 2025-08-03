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
import qualified Language.Haskell.TH.Syntax as TH

gitHead :: TH.Q TH.Exp
gitHead = do
  TH.addDependentFile ".git/HEAD"
  t <- TH.runIO readHead
  [|$(TH.lift t)|]

readHead :: IO Text
readHead = do
  txt <- Text.readFile ".git/HEAD"
  ref <- case Text.words txt of
    [ref] -> pure ref
    [ref, file] -> Text.readFile (".git/" <> Text.unpack file)
    _ -> fail $ "unexpected .git/HEAD: " <> Text.unpack txt
  pure (Text.filter (not . isSpace) ref)

gitHeadSym :: TH.Q TH.Type
gitHeadSym = do
  TH.addDependentFile ".git/HEAD"
  t <- TH.runIO readHead
  pure $ TH.LitT $ TH.StrTyLit $ Text.unpack t
