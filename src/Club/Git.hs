{-# LANGUAGE TemplateHaskell #-}

module Club.Git
  ( gitHead,
  )
where

import Data.ByteString as BS
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
    lift,
    runIO,
  )

gitHead :: Q Exp
gitHead = do
  typ <- [t|Text|]
  qAddDependentFile ".git/HEAD"
  e <- liftText =<< runIO readHead
  pure $ SigE e typ

readHead :: IO Text
readHead = do
  txt <- Text.readFile ".git/HEAD"
  case Text.words txt of
    [ref] -> pure ref
    [ref, file] -> Text.readFile (".git/" <> Text.unpack file)
    _ -> fail $ "unexpected .git/HEAD: " <> Text.unpack txt
