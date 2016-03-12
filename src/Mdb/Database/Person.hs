
{-# LANGUAGE
    DeriveDataTypeable,
    DeriveGeneric,
    OverloadedStrings,
    TemplateHaskell,
    TypeFamilies
    #-}

module Mdb.Database.Person (
    PersonId, Person(..)
    ) where

import           Database.SQLite.Simple ( FromRow(..), field )
import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import           Data.Typeable ( Typeable )
import           Generics.Generic.Aeson
import           GHC.Generics

type PersonId = Int64

data Person = Person
    { personId      :: ! PersonId
    , personName    :: ! String
    } deriving ( Eq, Generic, Show, Typeable )

instance FromRow Person where
    fromRow = Person <$> field <*> field

instance ToJSON Person where
  toJSON = gtoJson

instance FromJSON Person where
  parseJSON = gparseJson

instance JSONSchema Person where
  schema = gSchema
