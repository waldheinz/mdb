
module Route (
    Route(..), encode, decode,

    -- * Helpers
    clickRoute, goRoute
    ) where

import Effects exposing ( Effects )
import RouteParser exposing (..)
import TransitRouter

import Html
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Types exposing ( .. )

type Route
    = Home
    | Person PersonId
    | PersonAlbum PersonId AlbumId

routeParsers : List (Matcher Route)
routeParsers =
    [ static    Home        "/"
    , dyn1      Person      "/person/" int ""
    , dyn2      PersonAlbum "/person/" int "/album/" int ""
    ]

decode : String -> Route
decode path = RouteParser.match routeParsers path |> Maybe.withDefault Home

encode : Route -> String
encode route = case route of
    Home                -> "/"
    Person pid          -> "/person/" ++ toString pid
    PersonAlbum pid aid -> "/person/" ++ toString pid ++ "/album/" ++ toString aid

------------------------------------------------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------------------------------------------------

clickRoute : Route -> List Html.Attribute
clickRoute r =
    let
        path = encode r
    in
        [ HA.href path
        , HE.onWithOptions
            "click"
            { stopPropagation = True, preventDefault = True }
            (JD.succeed ())
            (\() -> Signal.message TransitRouter.pushPathAddress path)
        ]

goRoute : Route -> Effects ()
goRoute r = encode r
    |> Signal.send TransitRouter.pushPathAddress
    |> Effects.task
