
module Route exposing (
    Route(..), encode, decode,

    -- * Helpers
    Msg, clickRoute, clickRoute_, handleMsg
    )

import RouteParser exposing (..)

import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Navigation as NAV
import Types exposing ( .. )

type Route
    = Home
    | AlbumList
    | Album AlbumId (Maybe FileId)
    | Person PersonId
    | Series                            -- ^ series listing
    | SeriesSeasons SerialId            -- ^ individual serial
    | SeriesEpisodes SerialId SeasonId  -- ^ episodes in a season
    | Video FileId

routeParsers : List (Matcher Route)
routeParsers =
    [ static    Home                                "/"
    , static    AlbumList                           "/albums"
    , dyn1      (\aid -> Album aid Nothing)         "/album/"   int ""
    , dyn2      (\aid fid -> Album aid (Just fid))  "/album/"   int "/file/" int ""
    , dyn1      Person                              "/person/"  int ""
    , static    Series                              "/series"
    , dyn1      SeriesSeasons                       "/series/"  int ""
    , dyn2      SeriesEpisodes                      "/series/"  int "/season/"  int ""
    , dyn1      Video                               "/video/"   int ""
    ]

decode : String -> Route
decode path = RouteParser.match routeParsers path |> Maybe.withDefault Home

encode : Route -> String
encode route = case route of
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

type Msg
    = GoRoute Route

clickRoute : Route -> List (Html.Attribute Msg)
clickRoute = clickRoute_ identity

clickRoute_ : (Msg -> a) -> Route -> List (Html.Attribute a)
clickRoute_ wrap r =
    let
        path = encode r
    in
        [ HA.href path
        , HE.onWithOptions
            "click"
            { stopPropagation = True, preventDefault = True }
            (JD.succeed (wrap (GoRoute r)))
        ]

handleMsg : Msg -> Cmd never
handleMsg msg = case msg of
    GoRoute r -> NAV.newUrl (encode r)
