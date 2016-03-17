
module Server (
    ApiList,

    -- * Persons
    fetchPerson, fetchPersons, putPerson,

    -- * Albums
    WhichAlbums(..), fetchAlbums,

    -- * Files
    fetchFiles, fileThumbUrl, imageUrl, videoStreamUrl, videoFrameUrl,

    -- * Users / Login
    checkUser, doLogin,

    -- * Containers
    fetchContainerForFile
    ) where

import Http
import Json.Decode as JD exposing ( (:=) )
import Json.Encode as JE
import Task exposing ( Task )

import Album exposing ( albumListDecoder )
import Types exposing (..)

serverBaseUrl : String
serverBaseUrl = "" -- ^ yes, we want that relative for now

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

defaultRequest : List (String, String) -> String -> Http.Body -> String -> Http.Request
defaultRequest headers verb body endpoint =
    { verb      = verb
    , headers   = ( "Accept", "application/json" ) :: headers
    , url       = apiBaseUrl ++ endpoint
    , body      = body
    }

defaultGetRequest : String -> Http.Request
defaultGetRequest = defaultRequest [] "GET" Http.empty

defaultPutRequest : JE.Value -> String -> Http.Request
defaultPutRequest value = JE.encode 0 value |> Http.string
    |> defaultRequest [ ( "Content-Type", "application/json" ) ] "PUT"

------------------------------------------------------------------------------------------------------------------------
-- Persons
------------------------------------------------------------------------------------------------------------------------

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

fetchPerson : PersonId -> Task Http.Error Person
fetchPerson pid = defaultGetRequest ("/person/byId/" ++ toString pid)
    |> Http.send Http.defaultSettings
    |> Http.fromJson personDecoder

putPerson : PersonId -> Person -> Task Http.RawError Http.Response
putPerson pid p = defaultPutRequest (encodePerson pid p) ("/person/byId/" ++ toString pid)
    |> Http.send Http.defaultSettings

------------------------------------------------------------------------------------------------------------------------
-- Albums
------------------------------------------------------------------------------------------------------------------------

type WhichAlbums
    = AllAlbums
    | PersonAlbums PersonId

fetchAlbums : WhichAlbums -> Task Http.Error (ApiList (AlbumId, Album))
fetchAlbums which =
    let
        endpoint = case which of
            AllAlbums           -> "/album"
            PersonAlbums pid    -> "/person/byId/" ++ toString pid ++ "/albums"
    in
        defaultGetRequest endpoint
            |> Http.send Http.defaultSettings
            |> Http.fromJson (listDecoder albumListDecoder)

------------------------------------------------------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------------------------------------------------------

fetchFiles : WhichFiles -> Task Http.Error (ApiList (FileId, File))
fetchFiles which =
    let
        endpoint = case which of
            AllFiles        -> "/file"
            AlbumFiles aid  -> "/file/inAlbum/" ++ toString aid
            PersonNoAlbum pid   -> "/file/personNoAlbum/" ++ toString pid
    in
        defaultGetRequest endpoint
            |> Http.send Http.defaultSettings
            |> Http.fromJson (listDecoder fileListDecoder)

fetchContainerForFile : FileId -> Task Http.Error Container
fetchContainerForFile fid = defaultGetRequest ("/file/byId/" ++ toString fid ++ "/container")
    |> Http.send Http.defaultSettings
    |> Http.fromJson containerDecoder

------------------------------------------------------------------------------------------------------------------------
-- User / Login
------------------------------------------------------------------------------------------------------------------------

checkUser : Task Http.Error String
checkUser = Http.getString <| apiBaseUrl ++ "/user/self"

doLogin : { userName : String, password : String } -> Task Http.Error String
doLogin l =
    let
        json    = JE.object [ ( "user", JE.string l.userName ), ("pass", JE.string l.password ) ]
        body    = JE.encode 0 json |> Http.string
        url     = apiBaseUrl ++ "/user/self/login"
        decoder = JD.at ["user"] JD.string
        req     =
            { verb      = "POST"
            , headers   = [ ("Content-Type", "application/json" ) ]
            , url       = url
            , body      = body
            }
    in
        Http.send Http.defaultSettings req |> Http.fromJson decoder

------------------------------------------------------------------------------------------------------------------------
-- Media
------------------------------------------------------------------------------------------------------------------------

fileThumbUrl : FileId -> String
fileThumbUrl fid = serverBaseUrl ++ "/api/thumb/medium/" ++ toString fid

imageUrl : FileId -> String
imageUrl fid = serverBaseUrl ++ "/api/image/image/" ++ toString fid

videoStreamUrl : FileId -> String
videoStreamUrl fid = serverBaseUrl ++ "/api/video/" ++ toString fid ++ "/stream"

videoFrameUrl : FileId -> Float -> String
videoFrameUrl fid ts = serverBaseUrl ++ "/api/video/" ++ toString fid ++ "/frame?ts=" ++ toString ts
