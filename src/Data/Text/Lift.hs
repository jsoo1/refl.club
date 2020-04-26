{-# LANGUAGE TemplateHaskell #-}

module Data.Text.Lift (liftText, liftDataWithText) where

import Data.Data (Data, cast)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax (Exp (..), Lift, Q, dataToExpQ, lift)

instance Lift T.Text where
  lift = liftText

-- | https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

liftDataWithText :: Data a => a -> Q Exp
liftDataWithText = dataToExpQ (fmap liftText . cast)
