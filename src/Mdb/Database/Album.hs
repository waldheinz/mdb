
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Mdb.Database.Album (
    Album(..)
    ) where

import           Database.SQLite.Simple ( FromRow(..), field )
import           Data.Aeson
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import qualified Data.Text as T
import           Generics.Generic.Aeson
import           GHC.Generics

import           Mdb.Types

data Album = Album
    { albumId       :: ! AlbumId
    , albumName     :: ! T.Text
    , albumPoster   :: Maybe FileId
    , fileCount     :: ! Int
    } deriving ( Generic, Show )

instance FromRow Album where
    fromRow = Album <$> field <*> field <*> field <*> field

instance ToJSON Album where
  toJSON = gtoJson

instance FromJSON Album where
  parseJSON = gparseJson

instance JSONSchema Album where
  schema = gSchema
