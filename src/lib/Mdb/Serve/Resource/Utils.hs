
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Utils ( PlayTime(..), sortDir ) where

import           Database.SQLite.Simple     (Query)
import           Data.Aeson
import           Data.JSON.Schema           (JSONSchema (..), gSchema)
import           Generics.Generic.Aeson
import           GHC.Generics

sortDir :: Maybe String -> Query
sortDir (Just "DESC")   = "DESC"
sortDir _               = "ASC"

data PlayTime = PlayTime
    { playPos   :: Double
    , finished  :: Bool
    } deriving ( Generic, Show )

instance ToJSON PlayTime where
    toJSON = gtoJson

instance JSONSchema PlayTime where
    schema = gSchema
