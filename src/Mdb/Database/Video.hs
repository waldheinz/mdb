
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Mdb.Database.Video (
    VideoId, Video(..)
    ) where

import           Database.SQLite.Simple ( FromRow(..), field )
import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import qualified Data.Text as T
import           Generics.Generic.Aeson
import           GHC.Generics

type VideoId = Int64

data Video = Video
    { videoId   :: VideoId
    , duration  :: Double   -- ^ duration in seconds
    , format    :: T.Text   -- ^ container format ("avi", "mkv", ...)
    } deriving ( Generic, Show )

instance FromRow Video where
    fromRow = Video <$> field <*> field <*> field

instance ToJSON Video where
    toJSON = gtoJson

instance FromJSON Video where
    parseJSON = gparseJson

instance JSONSchema Video where
    schema = gSchema
