
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.File ( WithFile, fileResource ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT )
import           Control.Monad.Trans.Class ( lift )
import           Data.Monoid ( (<>) )
import           Rest
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.Album ( AlbumId )
import           Mdb.Database.File ( FileId, File )
import           Mdb.Database.Person ( PersonId )
import           Mdb.Serve.Auth as AUTH

data FileListSelector
    = AllFiles
    | FilesInAlbum AlbumId
    | PersonNoAlbum PersonId

type WithFile m = ReaderT FileId (Authenticated m)

fileResource :: (Applicative m, MonadIO m) => Resource (Authenticated m) (WithFile m) FileId FileListSelector Void
fileResource = R.Resource
    { R.name        = "file"
    , R.description = "Access file info"
    , R.schema      = withListing AllFiles schemas
    , R.list        = fileListHandler
    , R.get         = Just undefined
    } where
        schemas = named
            [ ( "inAlbum"       , listingBy (FilesInAlbum . read) )
            , ( "personNoAlbum" , listingBy (PersonNoAlbum . read) )
            , ( "byId"          , singleBy read)
            ]

fileListHandler :: MonadIO m => FileListSelector -> ListHandler (Authenticated m)
fileListHandler AllFiles = mkListing jsonO handler where
    handler r = lift $ listFiles (offset r) (count r)
fileListHandler (FilesInAlbum aid) = mkListing jsonO handler where
    handler _ = lift $ albumFiles aid
fileListHandler (PersonNoAlbum pid) = mkListing jsonO handler where
    handler _ = lift $ getRandomPersonFiles pid

listFiles
    :: MonadIO m
    => Int -- ^ offset
    -> Int -- ^ count
    -> Authenticated m [File]
listFiles off cnt = AUTH.query "SELECT file_id, file_name, file_size FROM file LIMIT ? OFFSET ?" (cnt, off)

albumFiles :: MonadIO m => AlbumId -> Authenticated m [File]
albumFiles aid = AUTH.query
    (   "SELECT f.file_id, f.file_name, f.file_size, file_mime FROM file f "
    <>  "NATURAL JOIN album_file "
    <>  "WHERE album_file.album_id = ? "
    <>  "ORDER BY f.file_name ASC" )
    (Only aid)

-- | Get files assigned to a person but not part of an album.
getRandomPersonFiles :: MonadIO m => PersonId -> Authenticated m [File]
getRandomPersonFiles pid = AUTH.query
    (   "SELECT DISTINCT f.file_id, f.file_name, f.file_size, f.file_mime FROM file f "
    <>  "NATURAL JOIN person_file "
    <>  "WHERE person_file.person_id = ? AND NOT EXISTS ("
    <>      "SELECT 1 FROM album a NATURAL JOIN person_file NATURAL JOIN album_file WHERE person_file.person_id = ? AND album_file.file_id = f.file_id "
    <>  ") LIMIT 100"
    )
    (pid, pid)
