{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( User (..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics

data User = User
  { username :: Text
  , password :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON User
