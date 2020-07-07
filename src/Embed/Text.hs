module Embed.Text where

import qualified Data.Text.IO as Text
import Data.Text.Lift ()
import Language.Haskell.TH.Syntax
  ( Exp (..),
    Lift,
    Q,
    Quasi (qAddDependentFile),
    lift,
    runIO,
  )

embedTextFile :: FilePath -> Q Exp
embedTextFile path = do
  qAddDependentFile path
  contents <- runIO $ Text.readFile path
  lift contents
