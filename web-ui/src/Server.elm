
module Server (
    ApiList,

    -- * Persons
    fetchPersons,

    -- * Albums
    WhichAlbums(..), fetchAlbums,

    -- * Files
    WhichFiles(..), fetchFiles
    ) where

import Http
import Json.Decode as JD exposing ( (:=) )
import Task exposing ( Task )

import Album exposing ( albumListDecoder )
import File exposing ( fileListDecoder )
import Person exposing ( personListDecoder )
import Types exposing (..)

apiBaseUrl : String
apiBaseUrl = "http://localhost:8080/api/0.1.0"

type alias ApiList a =
    { offset    : Int
    , count     : Int
    , items     : List a
    }

listDecoder : JD.Decoder a -> JD.Decoder (ApiList a)
listDecoder dec = JD.object3 ApiList
    ( "offset"  := JD.int )
    ( "count"   := JD.int )
    ( "items"   := JD.list dec )

defaultGetRequest : String -> Http.Request
defaultGetRequest endpoint =
    { verb      = "GET"
    , headers   = [("Accept", "application/json")]
    , url       = apiBaseUrl ++ endpoint
    , body      = Http.empty
    }

fetchPersons : Task Http.Error (ApiList (PersonId, Person))
fetchPersons = Http.fromJson (listDecoder personListDecoder) (Http.send Http.defaultSettings
    (defaultGetRequest "/person"))

type WhichAlbums
    = AllAlbums
    | PersonAlbums PersonId

fetchAlbums : WhichAlbums -> Task Http.Error (ApiList (AlbumId, Album))
fetchAlbums which =
    let
        endpoint = case which of
            AllAlbums           -> "/album"
            PersonAlbums pid    -> "/album/withPerson/" ++ toString pid
    in
        defaultGetRequest endpoint
            |> Http.send Http.defaultSettings
            |> Http.fromJson (listDecoder albumListDecoder)

type WhichFiles
    = AllFiles
    | AlbumFiles AlbumId

fetchFiles : WhichFiles -> Task Http.Error (ApiList (FileId, File))
fetchFiles which =
    let
        endpoint = case which of
            AllFiles        -> "/file"
            AlbumFiles aid  -> "/file/inAlbum/" ++ toString aid
    in
        defaultGetRequest endpoint
            |> Http.send Http.defaultSettings
            |> Http.fromJson (listDecoder fileListDecoder)
