
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Album (
    doAlbum
    ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Logger (logInfoN)
import qualified Database.SQLite.Simple as SQL
import Data.Monoid ( (<>) )
import qualified Data.Text as T

import Mdb.CmdLine ( OptAlbum(..) )
import Mdb.Database

doAlbum :: (MonadMask m, MonadIO m) => OptAlbum -> MDB m ()
doAlbum (AlbumCreate name) = isolate $ do
    dbExecute "INSERT INTO album (album_name) VALUES (?)" (SQL.Only name)
    dbLastRowId >>= \rid -> logInfoN $ "created with id " <> T.pack (show rid)
doAlbum AlbumRemoveEmpty = isolate $ do
    dbExecute_ $
        "DELETE FROM album WHERE album_id IN " <>
            "(select album_id from album a where not exists " <>
                "(select null from album_file af where af.album_id = a.album_id))"
    cnt <- withConnection $ \c -> liftIO (SQL.changes c)
    logInfoN $ "deleted " <> T.pack (show cnt) <> " albums"
