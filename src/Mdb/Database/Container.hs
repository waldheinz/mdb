
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Mdb.Database.Container (
    Container(..)
    ) where

import           Database.SQLite.Simple ( FromRow(..), field )
import           Data.Aeson
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import qualified Data.Text as T
import           Generics.Generic.Aeson
import           GHC.Generics

import           Mdb.Database.File ( FileId )

data Container = Container
    { fileId    :: FileId
    , duration  :: Double   -- ^ duration in seconds
    , format    :: T.Text   -- ^ container format ("avi", "mkv", ...)
    } deriving ( Generic, Show )

instance FromRow Container where
    fromRow = Container <$> field <*> field <*> field

instance ToJSON Container where
    toJSON = gtoJson

instance FromJSON Container where
    parseJSON = gparseJson

instance JSONSchema Container where
    schema = gSchema
