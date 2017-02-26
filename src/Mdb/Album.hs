
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Album (
    doAlbum
    ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Database.SQLite.Simple as SQL

import Mdb.CmdLine ( OptAlbum(..) )
import Mdb.Database

doAlbum :: (MonadMask m, MonadIO m) => OptAlbum -> MDB m ()
doAlbum (AlbumCreate name) = do
    dbExecute "INSERT INTO album (album_name) VALUES (?)" (SQL.Only name)
    dbLastRowId >>= \rid -> liftIO . putStrLn $ "created with id " ++ show rid
