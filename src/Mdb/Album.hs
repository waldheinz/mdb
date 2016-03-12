
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Album (
    doAlbum, albumFiles
    ) where

import Control.Monad.IO.Class ( liftIO )
import qualified Database.SQLite.Simple as SQL

import Mdb.CmdLine ( OptAlbum(..) )
import Mdb.Database

doAlbum :: OptAlbum -> MDB IO ()
doAlbum (AlbumCreate name) = do
    dbExecute "INSERT INTO album (album_name) VALUES (?)" (SQL.Only name)
    dbLastRowId >>= \rid -> liftIO . putStrLn $ "created with id " ++ show rid
