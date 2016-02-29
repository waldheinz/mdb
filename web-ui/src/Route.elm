
module Route (
    Route(..), encode, decode
    ) where

import RouteParser exposing (..)

type Route
    = Home

routeParsers : List (Matcher Route)
routeParsers =
    [ static Home "/"
    ]

decode : String -> Route
decode path = RouteParser.match routeParsers path |> Maybe.withDefault Home

encode : Route -> String
encode route = case route of
    Home -> "/"
