
module Server (
    -- * Persons
    fetchPerson, fetchPersons, putPerson,

    -- * Albums
    fetchAlbums, fetchAlbumInfo,

    -- * Files
    fetchFiles, fileThumbUrl, imageUrl, videoBaseUrl, videoStreamUrl, videoFrameUrl,

    -- * Users / Login
    checkUser, doLogin, doLogout, recordVideoPlay,

    -- * Containers
    fetchContainerForFile,

    -- * Serials
    fetchSerials, fetchSerialInfo, fetchSerialDescription, fetchSeasons, fetchEpisodes
    ) where

import Http
import Json.Decode as JD exposing ( (:=) )
import Json.Encode as JE
import Task exposing ( Task )

import Types exposing (..)

serverBaseUrl : String
serverBaseUrl = "" -- ^ yes, we want that relative for now

apiBaseUrl : String
apiBaseUrl = serverBaseUrl ++ "/api"

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

getJson : String -> JD.Decoder a -> Task Http.Error a
getJson ep dec = defaultGetRequest ep |> Http.send Http.defaultSettings |> Http.fromJson dec

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
        getJson endpoint (listDecoder personListDecoder)

fetchPerson : PersonId -> Task Http.Error Person
fetchPerson pid = getJson ("/person/byId/" ++ toString pid) personDecoder

putPerson : PersonId -> Person -> Task Http.RawError Http.Response
putPerson pid p = defaultPutRequest (encodePerson pid p) ("/person/byId/" ++ toString pid)
    |> Http.send Http.defaultSettings

------------------------------------------------------------------------------------------------------------------------
-- Albums
------------------------------------------------------------------------------------------------------------------------

fetchAlbums : WhichAlbums -> String -> String -> (Int, Int) -> Task Http.Error (ApiList Album)
fetchAlbums which order direction (offset, cnt) =
    let
        range =
            "?offset=" ++ toString offset ++
            "&count=" ++ toString cnt ++
            "&order=" ++ order ++
            "&direction=" ++ direction

        endpoint = case which of
            AllAlbums           -> "/album" ++ range
            PersonAlbums pid    -> "/person/byId/" ++ toString pid ++ "/albums" ++ range
    in
        getJson endpoint (listDecoder albumDecoder)

fetchAlbumInfo : AlbumId -> Task Http.Error Album
fetchAlbumInfo aid = getJson ("/album/byId/" ++ toString aid) albumDecoder

------------------------------------------------------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------------------------------------------------------

fetchFiles : WhichFiles -> (Int, Int) -> Task Http.Error (ApiList File)
fetchFiles which (offset, cnt) =
    let
        range =
            "?offset=" ++ toString offset ++
            "&count=" ++ toString cnt

        endpoint = case which of
            AllFiles        -> "/file" ++ range
            AlbumFiles aid  -> "/file/inAlbum/" ++ toString aid ++ range
            PersonNoAlbum pid   -> "/file/personNoAlbum/" ++ toString pid ++ range
    in
        getJson endpoint (listDecoder fileDecoder)

fetchContainerForFile : FileId -> Task Http.Error Container
fetchContainerForFile fid = getJson ("/file/byId/" ++ toString fid ++ "/container") containerDecoder

------------------------------------------------------------------------------------------------------------------------
-- Serials
------------------------------------------------------------------------------------------------------------------------

fetchSerials : SerialFilter -> Task never (Result Http.Error (ApiList (SerialId, Serial)))
fetchSerials flt =
    let
        ep = case flt of
            AllSerials  -> "/serial"
    in
        getJson ep (listDecoder serialListDecoder) |> Task.toResult

fetchSerialInfo : SerialId -> Task never (Result Http.Error Serial)
fetchSerialInfo sid = getJson ("/serial/" ++ toString sid) (serialDecoder) |> Task.toResult

fetchSerialDescription : SerialId -> Task never (Result Http.Error String)
fetchSerialDescription sid = Http.getString (apiBaseUrl ++ "/serial/" ++ toString sid ++ "/description") |> Task.toResult

fetchSeasons : SerialId -> Task never (Result Http.Error (ApiList Season))
fetchSeasons serid =
    getJson ("/serial/" ++ toString serid ++ "/season") (listDecoder seasonDecoder)
        |> Task.toResult

fetchEpisodes : SerialId -> SeasonId -> Task never (Result Http.Error (ApiList Episode))
fetchEpisodes serId seaId =
    getJson ("/serial/" ++ toString serId ++ "/season/" ++ toString seaId ++ "/episode") (listDecoder episodeDecoder)
        |> Task.toResult

------------------------------------------------------------------------------------------------------------------------
-- User / Login
------------------------------------------------------------------------------------------------------------------------

checkUser : Task Http.Error String
checkUser = Http.getString <| apiBaseUrl ++ "/user/self"

doLogin : { userName : String, password : String } -> Task Http.Error String
doLogin l =
    let
        json    = JE.object [ ( "user", JE.string l.userName ), ("pass", JE.string l.password ) ]
        decoder = JD.at ["userName"] JD.string
        req     =
            { verb      = "POST"
            , headers   = [ ("Content-Type", "application/json" ) ]
            , url       = apiBaseUrl ++ "/user/self/login"
            , body      = JE.encode 0 json |> Http.string
            }
    in
        Http.send Http.defaultSettings req |> Http.fromJson decoder

doLogout : Task Http.Error ()
doLogout =
    let
        req =
            { verb      = "POST"
            , headers   = [ ("Content-Type", "application/json" ) ]
            , url       = apiBaseUrl ++ "/user/self/logout"
            , body      = Http.empty
            }
    in
        Http.send Http.defaultSettings req |> Http.fromJson (JD.succeed ())

recordVideoPlay : JE.Value -> Task Http.Error ()
recordVideoPlay v =
    let
        req =
            { verb      = "POST"
            , headers   = [ ("Content-Type", "application/json" ) ]
            , url       = apiBaseUrl ++ "/user/self/videoPlay"
            , body      = JE.encode 0 v |> Http.string
            }
    in
        Http.send Http.defaultSettings req |> Http.fromJson (JD.succeed ())

------------------------------------------------------------------------------------------------------------------------
-- Media
------------------------------------------------------------------------------------------------------------------------

fileThumbUrl : FileId -> String
fileThumbUrl fid = serverBaseUrl ++ "/api/thumb/medium/" ++ toString fid

imageUrl : FileId -> String
imageUrl fid = serverBaseUrl ++ "/api/image/image/" ++ toString fid

videoBaseUrl : FileId -> String
videoBaseUrl fid = serverBaseUrl ++ "/api/video/" ++ toString fid

videoStreamUrl : FileId -> String
videoStreamUrl fid = serverBaseUrl ++ "/api/video/" ++ toString fid ++ "/stream"

videoFrameUrl : FileId -> Float -> String
videoFrameUrl fid ts = serverBaseUrl ++ "/api/video/" ++ toString fid ++ "/frame?ts=" ++ toString ts
