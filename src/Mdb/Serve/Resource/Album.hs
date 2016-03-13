
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Album ( albumResource ) where


import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Control.Monad.Trans.Class ( lift )
import           Data.Monoid ( (<>) )
import           Rest
import           Rest.Api ( Api, Router, Some1(..), route, root, mkVersion, (-/) )
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.Album ( AlbumId, Album )
import           Mdb.Database.File ( FileId, File )
import           Mdb.Database.Person ( PersonId, Person(..) )
import           Mdb.Serve.Auth as AUTH

data AlbumSelector
    = AllAlbums
    | AlbumWithPerson PersonId

type WithAlbum m = ReaderT AlbumId (MDB m)

albumResource :: (MonadIO m) => Resource (Authenticated m) (WithAlbum m) AlbumId AlbumSelector Void
albumResource = R.Resource
    { R.name        = "album"
    , R.description = "Access Albums"
    , R.schema      = withListing AllAlbums $ named schemas
    , R.get         = Just albumHandler
    , R.list        = albumListHandler
    }
    where
        schemas =
            [ ( "withPerson"    , listingBy (AlbumWithPerson . read))
            , ( "byId"          , singleBy read)
            ]

albumHandler :: MonadIO m => Handler (WithAlbum m)
albumHandler = mkIdHandler jsonO $ \() aid -> lift $ lift $ getAlbum aid

albumListHandler :: MonadIO m => AlbumSelector -> ListHandler (Authenticated m)
albumListHandler s = mkListing jsonO handler where
    handler r = lift $ case s of
        AllAlbums           -> getAlbums (offset r) (count r)
        AlbumWithPerson pid -> getPersonAlbums pid -- (offset r) (count r)

getAlbums :: MonadIO m => Int -> Int -> Authenticated m [Album]
getAlbums off cnt = AUTH.query
    (   "SELECT a.album_id, a.album_name, a.album_poster FROM album a "
    <>  "ORDER BY a.album_name LIMIT ?,?")
    (off, cnt)

-- | Get all albums containing files assigned to the given person.
getPersonAlbums :: MonadIO m => PersonId -> Authenticated m [Album]
getPersonAlbums pid = AUTH.query
    (   "SELECT DISTINCT a.album_id, a.album_name, a.album_poster FROM album a "
    <>  "NATURAL JOIN person_file "
    <>  "NATURAL JOIN album_file "
    <>  "WHERE person_file.person_id = ?" )
    (Only pid)
