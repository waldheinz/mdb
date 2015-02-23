
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

import           Control.Applicative ( (<*>), (<$>) )
import           Database.SQLite.Simple ( FromRow(..), field )
import qualified Data.ByteString as BS
import           Data.Aeson
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import           Data.Typeable ( Typeable )
import           Generics.Generic.Aeson
import           Generics.Regular ( deriveAll, PF )
import           Generics.Regular.XmlPickler ( gxpickle )
import           GHC.Generics
import           Text.XML.HXT.Arrow.Pickle ( XmlPickler(..) )

type FileId = Integer

data File = File
    { fileId    :: ! FileId
    , filePath  :: ! FilePath
    , fileSize  :: ! Integer
--    , fileSha1  :: ! (Maybe BS.ByteString)
    } deriving ( Eq, Generic, Show, Typeable )

instance FromRow File where
    fromRow = File <$> field <*> field <*> field

instance ToJSON File where
  toJSON = gtoJson

instance FromJSON File where
  parseJSON = gparseJson

instance JSONSchema File where
  schema = gSchema

$(deriveAll ''File "PFFile")
type instance PF File = PFFile

instance XmlPickler File where
    xpickle = gxpickle
