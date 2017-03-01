
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Utils ( sortDir ) where

import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Except ( ExceptT(..) )
import Database.SQLite.Simple ( Query )

import Mdb.Serve.Auth as AUTH

sortDir :: Maybe String -> Query
sortDir (Just "DESC")   = "DESC"
sortDir _               = "ASC"

query = ExceptT . AUTH.query
