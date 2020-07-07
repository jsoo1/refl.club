module Embed.Text where

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
