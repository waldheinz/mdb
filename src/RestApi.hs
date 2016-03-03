
{-# LANGUAGE OverloadedStrings #-}

module RestApi (
    apiApp
    ) where

import           Control.Applicative ( Applicative )
import           Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, bracket )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader ( ReaderT )
import           Control.Monad.Trans.Class ( lift )
import           Data.Aeson ( ToJSON(..), (.=) )
import qualified Data.Aeson as JSON
import           Network.HTTP.Types ( status200 )
import qualified Network.Wai as WAI
import           Network.Wai.Middleware.AddHeaders ( addHeaders )
import           Rest.Driver.Wai ( apiToApplication )
import           Rest
import           Rest.Api ( Api, Router, Some1(..), route, root, mkVersion, (-/) )
import qualified Rest.Resource as R

import           Database
import           Mdb.Database.Album ( AlbumId )
import           Mdb.Database.File ( FileId )
import           Mdb.Database.Person ( PersonId )

apiApp :: MediaDb -> WAI.Application
apiApp mdb = addHeaders [ ("Access-Control-Allow-Origin", "*") ] $
    apiToApplication (runMDB' mdb) api

api :: (Applicative m, MonadIO m) => Api (MDB m)
api = [(mkVersion 0 1 0, Some1 api010)]

api010 :: (Applicative m, MonadIO m) => Router (MDB m) (MDB m)
api010 = root
            -/ albums
            -/ files
            -/ persons
    where
        albums  = route albumResource
        files   = route fileResource
        persons = route personResource

-------------------------------------------------------------------------------
-- Files
-------------------------------------------------------------------------------

data FileListSelector
    = AllFiles
    | FilesInAlbum AlbumId

type WithFile m = ReaderT FileId m

fileResource :: (Applicative m, MonadIO m) => Resource (MDB m) (WithFile m) FileId FileListSelector Void
fileResource = R.Resource
    { R.name        = "file"
    , R.description = "Access file info"
    , R.schema      = withListing AllFiles $ named [("inAlbum", listingBy (FilesInAlbum . read))]
    , R.list        = fileListHandler
    }

fileListHandler :: MonadIO m => FileListSelector -> ListHandler (MDB m)
fileListHandler AllFiles = mkListing xmlJsonO handler where
    handler r = lift $ listFiles (offset r) (count r)
fileListHandler (FilesInAlbum aid) = mkListing xmlJsonO handler where
    handler _ = lift $ albumFiles aid

-------------------------------------------------------------------------------
-- Persons
-------------------------------------------------------------------------------

data PersonSelector = AllPersons

type WithPerson m = ReaderT PersonId m

personResource :: (Applicative m, MonadIO m) => Resource (MDB m) (WithPerson m) PersonId PersonSelector Void
personResource = R.Resource
    { R.name        = "person"
    , R.description = "Access persons"
    , R.schema      = withListing AllPersons $ named [("id", singleBy read)] -- ("id", listing
    , R.list        = personListHandler
    , R.get         = Nothing
    }

personListHandler :: MonadIO m => PersonSelector -> ListHandler (MDB m)
personListHandler AllPersons = mkListing xmlJsonO handler where
    handler r = lift $ listPersons (offset r) (count r)

-------------------------------------------------------------------------------
-- Albums
-------------------------------------------------------------------------------

data AlbumSelector
    = AllAlbums
    | AlbumWithPerson PersonId

type WithAlbum m = ReaderT AlbumId (MDB m)

albumResource :: (MonadIO m) => Resource (MDB m) (WithAlbum m) AlbumId AlbumSelector Void
albumResource = R.Resource
    { R.name        = "album"
    , R.description = "Access Albums"
    , R.schema      = withListing AllAlbums $ named [("withPerson", listingBy (AlbumWithPerson . read))]
    , R.list        = albumListHandler
    }

albumListHandler :: MonadIO m => AlbumSelector -> ListHandler (MDB m)
albumListHandler s = mkListing xmlJsonO handler where
    handler r = lift $ case s of
        AllAlbums           -> getAlbums (offset r) (count r)
        AlbumWithPerson pid -> getPersonAlbums pid -- (offset r) (count r)
