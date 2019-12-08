{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data.AWS.Error where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

class ToError a where
  toAWSError :: a -> Error

data Error
  = Error
      { errorType :: Text,
        errorMessage :: Text,
        stackTrace :: [Text]
      }
  deriving (Generic, ToJSON)
