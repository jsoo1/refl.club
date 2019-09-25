module Lib where

import GHC.Generics
import Lucid

import Aws.Lambda

data Person = Person
  { personName :: String
  , personAge :: Int
  } deriving (Generic, FromJSON, ToJSON)

handler :: Person -> Context -> IO (Either String Person)
handler person context =
  if personAge person > 0 then
    return (Right person)
  else
    return (Left "A person's age must be positive")
