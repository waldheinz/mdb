
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.RestApi (
    apiApp
    ) where

import           Control.Applicative ( Applicative )
import           Control.Monad ( (>=>) )
import           Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, bracket )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Control.Monad.Trans.Class ( lift )
import           Data.Aeson ( ToJSON(..), (.=) )
import qualified Data.Aeson as JSON
import           Network.HTTP.Types ( status200 )
import qualified Network.Wai as WAI
import           Rest.Driver.Wai ( apiToApplication )
import           Rest
import           Rest.Api ( Api, Router, Some1(..), route, root, mkVersion, (-/), (--/), (---/), (----/) )
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.Album ( AlbumId )
import           Mdb.Database.File ( FileId )
import           Mdb.Database.Person ( PersonId, Person(..) )
import           Mdb.Database.Video ( VideoId, Video(..) )
import           Mdb.Serve.Auth as AUTH
import           Mdb.Serve.Resource.Album ( albumResource )
import           Mdb.Serve.Resource.File ( fileResource )
import           Mdb.Serve.Resource.User ( userResource )
import           Mdb.Serve.Resource.Video ( videoResource )

apiApp :: MediaDb -> AUTH.SessionKey IO -> WAI.Application
apiApp mdb skey req = apiToApplication (runMDB' mdb . AUTH.request skey req) api req

api :: (Applicative m, MonadIO m) => Api (Authenticated m)
api = [(mkVersion 0 1 0, Some1 api010)]

api010 :: (Applicative m, MonadIO m) => Router (Authenticated m) (Authenticated m)
api010 = root
            -/ persons
                --/ albums
                    ---/ files
                        ----/ videos
                --/ files
                    ---/videos
    where
        albums  = route albumResource
        files   = route fileResource
        persons = route personResource
--        users   = route userResource
        videos  = route videoResource

-------------------------------------------------------------------------------
-- Persons
-------------------------------------------------------------------------------

data PersonSelector
    = AllPersons
    | InAlbum AlbumId

type WithPerson m = ReaderT PersonId (Authenticated m)

personResource :: MonadIO m => Resource (Authenticated m) (WithPerson m) PersonId PersonSelector Void
personResource = R.Resource
    { R.name        = "person"
    , R.description = "Access persons"
    , R.enter       = flip runReaderT
    , R.schema      = withListing AllPersons schemas
    , R.list        = personListHandler
    , R.private     = False
    , R.get         = Just $ mkConstHandler jsonO $ lift ask >>= \pid -> lift $ lift $ AUTH.unsafe $ getPerson pid
    , R.update      = Just updatePerson
    } where
        schemas = named
            [ ( "byId"      , singleBy read )
            , ( "inAlbum"   , listingBy (InAlbum . read) )
            ]

personListHandler :: MonadIO m => PersonSelector -> ListHandler (Authenticated m)
personListHandler which = mkListing jsonO handler where
    handler r = lift $ case which of
        AllPersons  -> AUTH.unsafe (listPersons (offset r) (count r))
        InAlbum aid -> AUTH.unsafe (getAlbumPersons aid)

updatePerson :: MonadIO m => Handler (WithPerson m)
updatePerson = mkInputHandler jsonI handler where
    handler p = do
        pid <- ask
        lift . lift $ AUTH.unsafe $ dbExecute
            "UPDATE person SET person_name = ? WHERE person_id = ?"
            (personName p, pid)

------------------------------------------------------------------------------------------------------------------------
-- Videos / Streams
------------------------------------------------------------------------------------------------------------------------
