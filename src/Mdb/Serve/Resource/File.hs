
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.File ( WithFile, fileResource, Container(..), Stream(..) ) where

import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.JSON.Schema           (JSONSchema (..), gSchema)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Database.SQLite.Simple     (FromRow (..), field)
import           Generics.Generic.Aeson
import           GHC.Generics
import           Rest
import qualified Rest.Resource              as R

import           Mdb.Database
import           Mdb.Database.File          (File)
import           Mdb.Serve.Auth             as AUTH
import           Mdb.Types

data FileListSelector
    = AllFiles
    | FilesInAlbum AlbumId
    | PersonNoAlbum PersonId

type WithFile m = ReaderT FileId (Authenticated m)

fileResource :: (MonadMask m, MonadIO m) => Resource (Authenticated m) (WithFile m) FileId FileListSelector Void
fileResource = mkResourceReader
    { R.name        = "file"
    , R.description = "Access file info"
    , R.schema      = withListing AllFiles schemas
    , R.list        = fileListHandler
    , R.get         = Nothing
    , R.selects     = [ ( "container", getContainerInfo )]
    } where
        schemas = named
            [ ( "inAlbum"       , listingBy (FilesInAlbum . read) )
            , ( "personNoAlbum" , listingBy (PersonNoAlbum . read) )
            , ( "byId"          , singleBy read)
            ]
fileListHandler :: (MonadMask m, MonadIO m) => FileListSelector -> ListHandler (Authenticated m)
fileListHandler which = mkOrderedListing jsonO handler where
    handler (r, o, d) = case which of
        AllFiles            -> lift $ listFiles (offset r) (count r)
        FilesInAlbum aid    -> lift $
            AUTH.query
                (   "SELECT f.file_id, f.file_name, f.file_size, file_mime FROM auth_file f "
                <>  "NATURAL JOIN album_file "
                <>  "WHERE album_file.album_id = ? "
                <>  "ORDER BY f.file_name ASC "
                <>  "LIMIT ? OFFSET ?"
                ) (aid, count r, offset r)
        PersonNoAlbum pid   -> lift $ getRandomPersonFiles pid

listFiles
    :: (MonadMask m, MonadIO m)
    => Int -- ^ offset
    -> Int -- ^ count
    -> Authenticated m [File]
listFiles off cnt = AUTH.query "SELECT file_id, file_name, file_size FROM auth_file LIMIT ? OFFSET ?" (cnt, off)

-- | Get files assigned to a person but not part of an album.
getRandomPersonFiles :: (MonadMask m, MonadIO m) => PersonId -> Authenticated m [File]
getRandomPersonFiles pid = AUTH.query
    (   "SELECT DISTINCT f.file_id, f.file_name, f.file_size, f.file_mime FROM auth_file f "
    <>  "NATURAL JOIN person_file "
    <>  "WHERE person_file.person_id = ? AND NOT EXISTS ("
    <>      "SELECT 1 FROM auth_album a NATURAL JOIN person_file NATURAL JOIN album_file WHERE person_file.person_id = ? AND album_file.file_id = f.file_id "
    <>  ") LIMIT 100"
    )
    (pid, pid)

------------------------------------------------------------------------------------------------------------------------
-- Containers
------------------------------------------------------------------------------------------------------------------------

data Stream = Stream
    { streamId        :: Int
    , streamMediaType :: T.Text
    , streamCodec     :: T.Text
    , streamBitRate   :: Int
    , streamWidth     :: Int
    , streamHeight    :: Int
    } deriving ( Generic, Show )

instance ToJSON Stream where
    toJSON = gtoJson

instance JSONSchema Stream where
    schema = gSchema

instance FromRow Stream where
    fromRow = Stream <$> field <*> field <*> field <*> field <*> field <*> field

data Container = Container
    { duration :: Double   -- ^ duration in seconds
    , format   :: T.Text   -- ^ container format ("avi", "mkv", ...)
    , streams  :: [Stream]
    } deriving ( Generic, Show )

instance ToJSON Container where
    toJSON = gtoJson

instance JSONSchema Container where
    schema = gSchema

getContainerInfo :: (MonadMask m, MonadIO m) => Handler (WithFile m)
getContainerInfo = mkIdHandler jsonO handler where
    handler :: (MonadMask m, MonadIO m) => () -> FileId -> ExceptT Reason_ (WithFile m) Container
    handler () fid = do
        (d, fmt) <- ExceptT $ lift $ AUTH.queryOne
            "SELECT container_duration, container_format FROM container WHERE file_id=?" (Only fid)

        ss <- lift . lift $ AUTH.query
            ("SELECT stream_id, stream_media_type, stream_codec, stream_bit_rate, stream_width, stream_height " <>
            "FROM stream WHERE file_id=?") (Only fid)

        return $ Container d fmt ss
