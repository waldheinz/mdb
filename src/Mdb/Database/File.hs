
{-# LANGUAGE
    DeriveDataTypeable,
    DeriveGeneric,
    OverloadedStrings,
    TemplateHaskell,
    TypeFamilies
    #-}

module Mdb.Database.File (
    FileId, File(..)
    ) where

import           Database.SQLite.Simple ( FromRow(..), field )
import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import qualified Data.Text as T
import           Data.Typeable ( Typeable )
import           Generics.Generic.Aeson
import           GHC.Generics

type FileId = Int64

data File = File
    { fileId    :: ! FileId
    , filePath  :: ! FilePath
    , fileSize  :: ! Integer
    , fileMime  :: ! T.Text
    } deriving ( Eq, Generic, Show, Typeable )

instance FromRow File where
    fromRow = File <$> field <*> field <*> field <*> field

instance ToJSON File where
  toJSON = gtoJson

instance FromJSON File where
  parseJSON = gparseJson

instance JSONSchema File where
  schema = gSchema
