{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Org.Instances where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty(..))
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
import Data.Text.Lift
import Language.Haskell.TH.Syntax
       ( Exp(..),
         Lift,
         Q,
         lift,
         runIO,
         Quasi(qAddDependentFile)
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
