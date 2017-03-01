
{-# LANGUAGE DeriveGeneric #-}

module Mdb.Database.Person (
    Person(..)
    ) where

import           Data.Aeson
import           Data.JSON.Schema       (JSONSchema (..), gSchema)
import           Database.SQLite.Simple (FromRow (..), field)
import           Generics.Generic.Aeson
import           GHC.Generics

import           Mdb.Types

data Person = Person
    { personId       :: ! PersonId
    , personName     :: ! String
    , personPortrait :: Maybe FileId
    } deriving ( Generic, Show )

instance FromRow Person where
    fromRow = Person <$> field <*> field <*> field

instance ToJSON Person where
  toJSON = gtoJson

instance FromJSON Person where
  parseJSON = gparseJson

instance JSONSchema Person where
  schema = gSchema
