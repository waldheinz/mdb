
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
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import           Data.Typeable ( Typeable )
import           Generics.Generic.Aeson
import           Generics.Regular ( deriveAll, PF )
import           Generics.Regular.XmlPickler ( gxpickle )
import           GHC.Generics
import           Text.XML.HXT.Arrow.Pickle ( XmlPickler(..) )

type PersonId = Integer

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

$(deriveAll ''Person "PFPerson")
type instance PF Person = PFPerson

instance XmlPickler Person where
    xpickle = gxpickle
