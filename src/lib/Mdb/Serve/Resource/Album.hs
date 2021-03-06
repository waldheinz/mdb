
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Album ( albumResource, personAlbumResource ) where

import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Data.Monoid                ((<>))
import           Database.SQLite.Simple     (Query)
import           Rest
import qualified Rest.Resource              as R

import           Mdb.Database
import           Mdb.Database.Album         (Album)
import           Mdb.Serve.Auth             as AUTH
import           Mdb.Serve.Resource.Person  (WithPerson)
import           Mdb.Serve.Resource.Utils   (sortDir)
import           Mdb.Types

type WithAlbum m = ReaderT AlbumId (Authenticated m)

-- |
-- We basically "forget" that we found that album through a person, because it doesn't matter.
enter :: Monad m => AlbumId -> WithAlbum m b -> WithPerson m b
enter aid f = lift $ runReaderT f aid

personAlbumResource :: (MonadMask m, MonadIO m) => Resource (WithPerson m) (WithAlbum m) AlbumId () Void
personAlbumResource = (mkResource enter)
    { R.name    = "albums"
    , R.schema  = withListing () $ named []
    , R.list    = const listPersonAlbums
    }

posterQuery :: Query
posterQuery =
    "COALESCE(" <>
        "a.album_poster," <>
            "(SELECT file_id FROM auth_file NATURAL JOIN album_file af " <>
                "WHERE af.album_id = a.album_id ORDER BY auth_file.file_name LIMIT 1)" <>
    ") "

fileCountQuery :: Query
fileCountQuery =
    "(" <>
        "SELECT COUNT(file_id) AS file_count FROM album_file WHERE album_id = a.album_id" <>
    ") "

listPersonAlbums :: (MonadMask m, MonadIO m) => ListHandler (WithPerson m)
listPersonAlbums = mkListing jsonO handler
    where
        handler :: (MonadMask m, MonadIO m) => Range -> ExceptT Reason_ (WithPerson m) [Album]
        handler r = do
            pid <- lift ask
            lift . lift $ AUTH.query
                (   "SELECT DISTINCT a.album_id, a.album_name, " <> posterQuery <> "," <> fileCountQuery
                <>  "FROM auth_album a "
                <>  "NATURAL JOIN person_file "
                <>  "NATURAL JOIN album_file "
                <>  "WHERE person_file.person_id = ? LIMIT ?,?" )
                (pid, offset r, count r)

albumOrder :: Maybe String -> Query
albumOrder Nothing     = "album_name"
albumOrder (Just o)    = case o of
    "created"   -> "album_created"
    _           -> "album_name"

albumResource :: (MonadMask m, MonadIO m) => Resource (Authenticated m) (WithAlbum m) AlbumId () Void
albumResource = mkResourceReader
    { R.name        = "album"
    , R.schema      = withListing () $ named [ ( "byId", singleBy read ) ]
    , R.get         = Just albumHandler
    , R.list        = const listAlbums
    }

albumHandler :: (MonadMask m, MonadIO m) => Handler (WithAlbum m)
albumHandler = mkIdHandler jsonO handler where
    handler :: (MonadMask m, MonadIO m) => () -> AlbumId -> ExceptT Reason_ (WithAlbum m) Album
    handler () aid = ExceptT $ lift $ AUTH.queryOne
            (   "SELECT a.album_id, a.album_name, " <> posterQuery <> ", " <> fileCountQuery
            <>  "FROM auth_album a WHERE a.album_id = ?")
            (Only aid)

listAlbums :: (MonadMask m, MonadIO m) => ListHandler (Authenticated m)
listAlbums = mkOrderedListing jsonO handler where
    handler :: (MonadMask m, MonadIO m) => (Range, Maybe String, Maybe String) -> ExceptT Reason_ (Authenticated m) [Album]
    handler (r, o, d) = lift $ AUTH.query
        (   "SELECT a.album_id, a.album_name, " <> posterQuery <> ", " <> fileCountQuery
        <>  "FROM auth_album a "
        <>  "ORDER BY " <> albumOrder o <> " " <> sortDir d <> " "
        <>  "LIMIT ?,?") (offset r, count r)
