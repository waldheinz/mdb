
module Server exposing (
    -- * Albums
    fetchAlbums,

    -- * Files
    fetchFiles,

    -- * Media
    fileThumbUrl

    -- * Users / Login
    -- checkUser, doLogin, doLogout
    )

import Json.Decode as JD
import Http

import Types exposing ( .. )

type alias Model =
    {
    }

------------------------------------------------------------------------------------------------------------------------
-- Albums
------------------------------------------------------------------------------------------------------------------------

fetchAlbums : WhichAlbums -> String -> String -> (Int, Int) -> Http.Request (ApiList Album)
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
        Http.get endpoint (listDecoder albumDecoder)

------------------------------------------------------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------------------------------------------------------

fetchFiles : WhichFiles -> (Int, Int) -> Http.Request (ApiList File)
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
        Http.get endpoint (listDecoder fileDecoder)

------------------------------------------------------------------------------------------------------------------------
-- Media
------------------------------------------------------------------------------------------------------------------------

fileThumbUrl : FileId -> String
fileThumbUrl fid = "/api/thumb/medium/" ++ toString fid
