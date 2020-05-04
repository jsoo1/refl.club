{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

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

module Data.Org.Instances where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Org
  ( Block (..),
    Column (..),
    Item (..),
    Language (..),
    ListItems (..),
    OrgDoc (..),
    OrgFile (..),
    Row (..),
    Section (..),
    URL (..),
    Words (..),
    org,
  )
import Data.Text.Lift
import Language.Haskell.TH.Syntax
  ( Exp (..),
    Lift,
    Q,
    Quasi (qAddDependentFile),
    lift,
    runIO,
  )

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
