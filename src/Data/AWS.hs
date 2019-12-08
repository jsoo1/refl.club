{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Data.AWS where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

class ToAWSError a where
  toAWSError :: a -> Error

data Error
  = Error
      { errorType :: Text,
        errorMessage :: Text,
        stackTrace :: [Text]
      }
  deriving (Generic, ToJSON)

newtype StatusResponse
  = StatusResponse
      { status :: Text
      }
  deriving (Generic, ToJSON, FromJSON)

data ErrorResponse
  = ErrorResponse
      { errorMessage :: Text,
        errorType :: Text
      }
  deriving (Generic, ToJSON, FromJSON)

type EventResponse = Object
