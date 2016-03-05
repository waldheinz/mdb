
module Server (
    ApiList,

    -- * Persons
    fetchPersons,

    -- * Albums
    WhichAlbums(..), fetchAlbums,

    -- * Files
    fetchFiles, fileThumbUrl, imageUrl, videoStreamUrl
    ) where

import Http
import Json.Decode as JD exposing ( (:=) )
import Task exposing ( Task )

import Album exposing ( albumListDecoder )
import Types exposing (..)

serverBaseUrl : String
serverBaseUrl = "http://localhost:8080"

apiBaseUrl : String
apiBaseUrl = serverBaseUrl ++ "/api/0.1.0"

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

fetchPersons : PersonFilter -> Task Http.Error (ApiList (PersonId, Person))
fetchPersons which =
    let
        endpoint = case which of
            AllPersons          -> "/person"
            AlbumPersons aid    -> "/person/inAlbum/" ++ toString aid
    in
        defaultGetRequest endpoint
            |> Http.send Http.defaultSettings
            |> Http.fromJson (listDecoder personListDecoder)

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

fileThumbUrl : FileId -> String
fileThumbUrl fid = serverBaseUrl ++ "/image/thumbnail/" ++ toString fid

imageUrl : FileId -> String
imageUrl fid = serverBaseUrl ++ "/image/image/" ++ toString fid

videoStreamUrl : FileId -> String
videoStreamUrl fid = serverBaseUrl ++ "/video/streamDirect/" ++ toString fid
