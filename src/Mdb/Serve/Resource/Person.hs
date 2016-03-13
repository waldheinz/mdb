
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Person (
    WithPerson, personResource
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT, ask )
import           Control.Monad.Trans.Class ( lift )
import           Rest
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.Album ( AlbumId )
import           Mdb.Database.Person ( PersonId, Person(..) )
import           Mdb.Serve.Auth as AUTH

data PersonSelector
    = AllPersons
    | InAlbum AlbumId

type WithPerson m = ReaderT PersonId (Authenticated m)

personResource :: MonadIO m => Resource (Authenticated m) (WithPerson m) PersonId PersonSelector Void
personResource = mkResourceReader
    { R.name        = "person"
    , R.description = "Access persons"
    , R.schema      = withListing AllPersons schemas
    , R.list        = personListHandler
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
