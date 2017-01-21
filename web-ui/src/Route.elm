
module Route exposing (
    Route(..), parse, clickRoute, go
    )

import Json.Decode as JD
import Html
import Html.Attributes as HA
import Html.Events as HE
import UrlParser exposing (..)

import Navigation as NAV
import Types exposing ( .. )

type Route
    = NotFound NAV.Location
    | Home
    | AlbumList
    | Album AlbumId (Maybe FileId)
    | Person PersonId
    | Series                            -- ^ series listing
    | SeriesSeasons SerialId            -- ^ individual serial
    | SeriesEpisodes SerialId SeasonId  -- ^ episodes in a season
    | Video FileId

parse : NAV.Location -> Route
parse loc = case parsePath route loc of
    Just r  -> r
    Nothing -> NotFound loc

route : Parser (Route -> a) a
route = oneOf
    [ map Home top
    ]

encode : Route -> String
encode route = case route of
    NotFound loc            -> "/404"
    Home                    -> "/"
    AlbumList               -> "/albums"
    Album aid Nothing       -> "/album/" ++ toString aid
    Album aid (Just fid)    -> "/album/" ++ toString aid ++ "/file/" ++ toString fid
    Person pid              -> "/person/" ++ toString pid
    Series                  -> "/series"
    SeriesSeasons sid       -> "/series/" ++ toString sid
    SeriesEpisodes r a      -> "/series/" ++ toString r ++ "/season/" ++ toString a
    Video fid               -> "/video/" ++ toString fid

-- routeParser =
--    (NAV.makeParser (\_ -> Home))

------------------------------------------------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------------------------------------------------

clickRoute : Route -> List (Html.Attribute Route)
clickRoute r =
    let
        path = encode r
    in
        [ HA.href path
        , HE.onWithOptions
            "click"
            { stopPropagation = True, preventDefault = True }
            (JD.succeed r)
        ]

go : Route -> Cmd never
go r = NAV.newUrl (encode r)
