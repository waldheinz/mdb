
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Mdb.Serve.Resource.Serial (
    serialResource
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT, ask )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Except
import           Database.SQLite.Simple ( FromRow(..), field, Query )
import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Generics.Generic.Aeson
import           GHC.Generics
import           Rest
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.File ( FileId )
import           Mdb.Serve.Auth as AUTH

------------------------------------------------------------------------------------------------------------------------
-- types
------------------------------------------------------------------------------------------------------------------------

type SerialId = Int64

type WithSerial m = ReaderT SerialId (Authenticated m)

data SerialFilter = AllSerials

data Serial = Serial
    { serialId      :: SerialId
    , serialName    :: T.Text
    , serialPoster  :: Maybe FileId
    } deriving ( Generic, Show )

instance FromRow Serial where
    fromRow = Serial <$> field <*> field <*> field

instance ToJSON Serial where
  toJSON = gtoJson

instance JSONSchema Serial where
  schema = gSchema

------------------------------------------------------------------------------------------------------------------------
-- serials
------------------------------------------------------------------------------------------------------------------------

serialResource :: MonadIO m => Resource (Authenticated m) (WithSerial m) SerialId SerialFilter Void
serialResource = mkResourceReader
    { R.name        = "serial"
    , R.description = "Access TV serials"
    , R.schema      = withListing AllSerials $ named []
    , R.list        = serialList
    }

serialOrder :: Maybe String -> Query
serialOrder Nothing     = "series_name"
serialOrder (Just o)    = case o of
    "name"  -> "series_name"
    _       -> "series_name"

sortDir :: Maybe String -> Query
sortDir (Just "DESC")   = "DESC"
sortDir _               = "ASC"

serialList :: MonadIO m => SerialFilter -> ListHandler (Authenticated m)
serialList which = mkOrderedListing jsonO handler where
    handler :: MonadIO m => (Range, Maybe String, Maybe String) -> ExceptT Reason_ (Authenticated m) [Serial]
    handler (r, o, d) = case which of
        AllSerials  -> lift $
            AUTH.query
                (   "SELECT series_id, series_name, series_poster FROM series "
                <>  "ORDER BY " <> serialOrder o <> " " <> sortDir d <> " "
                <>  "LIMIT ? OFFSET ?" ) (count r, offset r)
