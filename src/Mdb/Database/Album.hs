
{-# LANGUAGE
    DeriveDataTypeable,
    DeriveGeneric,
    OverloadedStrings,
    TemplateHaskell,
    TypeFamilies
    #-}

module Mdb.Database.Album (
    AlbumId, Album(..)
    ) where

import           Database.SQLite.Simple ( FromRow(..), field )
import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import qualified Data.Text as T
import           Data.Typeable ( Typeable )
import           Generics.Generic.Aeson
import           GHC.Generics

import           Mdb.Database.File ( FileId )

type AlbumId = Int64

data Album = Album
    { albumId       :: ! AlbumId
    , albumName     :: ! T.Text
    , albumPoster   :: Maybe FileId
    } deriving ( Eq, Generic, Show, Typeable )

instance FromRow Album where
    fromRow = Album <$> field <*> field <*> field

instance ToJSON Album where
  toJSON = gtoJson

instance FromJSON Album where
  parseJSON = gparseJson

instance JSONSchema Album where
  schema = gSchema
