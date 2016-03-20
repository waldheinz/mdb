
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Album ( albumResource, personAlbumResource ) where

import           Control.Monad.Error.Class ( throwError )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Except
import           Database.SQLite.Simple (Query)
import           Data.Monoid ( (<>) )
import           Rest
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.Album ( AlbumId, Album )
import           Mdb.Serve.Auth as AUTH
import           Mdb.Serve.Resource.Person ( WithPerson )

type WithAlbum m = ReaderT AlbumId (Authenticated m)

-- |
-- We basically "forget" that we found that album through a person, because it doesn't matter.
enter :: Monad m => AlbumId -> WithAlbum m b -> WithPerson m b
enter aid f = lift $ runReaderT f aid

personAlbumResource :: MonadIO m => Resource (WithPerson m) (WithAlbum m) AlbumId () Void
personAlbumResource = (mkResource enter)
    { R.name    = "albums"
    , R.schema  = withListing () $ named []
    , R.list    = const listPersonAlbums
    }

posterQuery :: Query
posterQuery = "COALESCE(a.album_poster, (SELECT af.file_id FROM album_file af WHERE af.album_id = a.album_id LIMIT 1))"

listPersonAlbums :: MonadIO m => ListHandler (WithPerson m)
listPersonAlbums = mkListing jsonO handler
    where
        handler :: MonadIO m => Range -> ExceptT Reason_ (WithPerson m) [Album]
        handler r = do
            pid <- lift ask
            lift . lift $ AUTH.query
                (   "SELECT DISTINCT a.album_id, a.album_name, " <> posterQuery <> " FROM album a "
                <>  "NATURAL JOIN person_file "
                <>  "NATURAL JOIN album_file "
                <>  "WHERE person_file.person_id = ? LIMIT ?,?" )
                (pid, offset r, count r)

albumResource :: (MonadIO m) => Resource (Authenticated m) (WithAlbum m) AlbumId () Void
albumResource = mkResourceReader
    { R.name        = "album"
    , R.schema      = withListing () $ named [ ( "byId", singleBy read ) ]
    , R.get         = Just albumHandler
    , R.list        = const listAlbums
    }

albumHandler :: MonadIO m => Handler (WithAlbum m)
albumHandler = mkIdHandler jsonO handler where
    handler :: MonadIO m => () -> AlbumId -> ExceptT Reason_ (WithAlbum m) Album
    handler () aid = do
        al <- lift . lift $ AUTH.query
            ("SELECT a.album_id, a.album_name, " <> posterQuery <> " FROM album a WHERE a.album_id = ?")
            (Only aid)

        case al of
            []  -> throwError NotFound
            (a : _)  -> return a

listAlbums :: MonadIO m => ListHandler (Authenticated m)
listAlbums = mkListing jsonO handler where
    handler :: MonadIO m => Range -> ExceptT Reason_ (Authenticated m) [Album]
    handler r = lift $ AUTH.query
        (   "SELECT a.album_id, a.album_name, " <> posterQuery <> " "
        <>  "FROM album a "
        <>  "ORDER BY a.album_name LIMIT ?,?") (offset r, count r)
