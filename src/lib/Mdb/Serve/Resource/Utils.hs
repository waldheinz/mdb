
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Utils ( PlayProgress(..), sortDir ) where

import           Database.SQLite.Simple     (Query)
import           Data.Aeson
import           Data.JSON.Schema           (JSONSchema (..), gSchema)
import           Generics.Generic.Aeson
import           GHC.Generics

sortDir :: Maybe String -> Query
sortDir (Just "DESC")   = "DESC"
sortDir _               = "ASC"

data PlayProgress = PlayProgress
    { playProgressPos       :: Double
    , playProgressFinished  :: Bool
    } deriving ( Generic, Show )

instance ToJSON PlayProgress where
    toJSON = gtoJson

instance JSONSchema PlayProgress where
    schema = gSchema
