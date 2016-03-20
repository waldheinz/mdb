
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Utils ( sortDir ) where

import Database.SQLite.Simple ( Query)

sortDir :: Maybe String -> Query
sortDir (Just "DESC")   = "DESC"
sortDir _               = "ASC"
