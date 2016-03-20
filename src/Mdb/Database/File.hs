
{-# LANGUAGE DeriveGeneric #-}

module Mdb.Database.File (
    File(..)
    ) where

import           Data.Aeson
import           Data.JSON.Schema       (JSONSchema (..), gSchema)
import qualified Data.Text              as T
import           Database.SQLite.Simple (FromRow (..), field)
import           Generics.Generic.Aeson
import           GHC.Generics

import           Mdb.Types

data File = File
    { fileId   :: ! FileId
    , filePath :: ! FilePath
    , fileSize :: ! Integer
    , fileMime :: ! T.Text
    } deriving ( Generic, Show )

instance FromRow File where
    fromRow = File <$> field <*> field <*> field <*> field

instance ToJSON File where
    toJSON = gtoJson

instance FromJSON File where
    parseJSON = gparseJson

instance JSONSchema File where
    schema = gSchema
