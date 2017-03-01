
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Person (
    WithPerson, personResource
    ) where

import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (ReaderT, ask)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Data.Monoid                ((<>))
import           Database.SQLite.Simple     (Query)
import           Rest
import qualified Rest.Resource              as R

import           Mdb.Database
import           Mdb.Database.Person        (Person (..))
import           Mdb.Serve.Auth             as AUTH
import           Mdb.Serve.Resource.Utils   (sortDir)
import           Mdb.Types

data PersonSelector
    = AllPersons
    | InAlbum AlbumId

type WithPerson m = ReaderT PersonId (Authenticated m)

personResource :: (MonadMask m, MonadIO m) => Resource (Authenticated m) (WithPerson m) PersonId PersonSelector Void
personResource = mkResourceReader
    { R.name        = "person"
    , R.description = "Access persons"
    , R.schema      = withListing AllPersons schemas
    , R.list        = personListHandler
    , R.get         = Just getPerson
    , R.update      = Just updatePerson
    } where
        schemas = named
            [ ( "byId"      , singleBy read )
            , ( "inAlbum"   , listingBy (InAlbum . read) )
            ]

getPerson :: (MonadMask m, MonadIO m) => Handler (WithPerson m)
getPerson = mkIdHandler jsonO handler where
    handler :: (MonadMask m, MonadIO m) => () -> SerialId -> ExceptT Reason_ (WithPerson m) Person
    handler () pid = ExceptT $ lift $ AUTH.queryOne
        "SELECT person_id, person_name, person_portrait FROM auth_person WHERE (person_id = ?)"
        (Only pid)

personOrder :: Maybe String -> Query
personOrder Nothing     = "person_name"
personOrder (Just o)    = case o of
    "name"  -> "person_name"
    _       -> "person_name"

personListHandler :: (MonadMask m, MonadIO m) => PersonSelector -> ListHandler (Authenticated m)
personListHandler which = mkOrderedListing jsonO handler where
    handler :: (MonadMask m, MonadIO m) => (Range, Maybe String, Maybe String) -> ExceptT Reason_ (Authenticated m) [Person]
    handler (r, o, d) = ExceptT $ lift $ case which of
        AllPersons  -> AUTH.query
                        (   "SELECT person_id, person_name, person_portrait FROM auth_person "
                        <>  "ORDER BY " <> personOrder o <> " " <> sortDir d <> " "
                        <>  "LIMIT ? OFFSET ?"
                        ) (count r, offset r)

        InAlbum aid -> AUTH.query
                        ( "SELECT DISTINCT p.person_id, p.person_name, person_portrait FROM auth_person p "
                        <>  "NATURAL JOIN person_file "
                        <>  "WHERE person_file.person_id = p.person_id AND EXISTS ("
                        <>      "SELECT 1 FROM auth_album a NATURAL JOIN person_file NATURAL JOIN album_file "
                        <>          "WHERE person_file.person_id = p.person_id AND album_file.album_id = ?)"
                        <>  "ORDER BY " <> personOrder o <> " " <> sortDir d <> " "
                        ) (Only aid)

updatePerson :: (MonadMask m, MonadIO m) => Handler (WithPerson m)
updatePerson = mkInputHandler jsonI handler where
    handler p = do
        pid <- ask
        lift . lift $ AUTH.unsafe $ dbExecute
            "UPDATE person SET person_name = ? WHERE person_id = ?"
            (personName p, pid)
