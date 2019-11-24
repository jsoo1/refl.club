{-# LANGUAGE DeriveAnyClass #-}

module Lib where

import Data.Aeson
import GHC.Generics
import Lucid

data Person
  = Person {personName :: String, personAge :: Int}
  deriving (Generic, FromJSON, ToJSON)

handler :: Person -> IO (Either String Person)
handler person =
  if personAge person > 0
    then return (Right person)
    else return (Left "A person's age must be positive")
