
module Route (
    Route(..), encode, decode
    ) where

import RouteParser exposing (..)

import Types exposing ( PersonId )

type Route
    = Home
    | Person PersonId

routeParsers : List (Matcher Route)
routeParsers =
    [ static    Home    "/"
    , dyn1      Person  "/person/" int ""
    ]

decode : String -> Route
decode path = RouteParser.match routeParsers path |> Maybe.withDefault Home

encode : Route -> String
encode route = case route of
    Home        -> "/"
    Person pid  -> "/person/" ++ toString pid
