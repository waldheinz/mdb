
module Listing exposing (
    Model, FetchTask, mkModel, pagination,
    Action, withFetchTask, withItemCount, refresh, update
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Task exposing ( Task )

import Types exposing ( .. )

type alias FetchTask a = (Int, Int) -> Http.Request (ApiList a)

type alias Model a =
    { items         : List a
    , perPage       : Int
    , currentPage   : Int
    , totalItems    : Maybe Int
    , fetch         : FetchTask a
    }

mkModel : FetchTask a -> Model a
mkModel f =
    { items = []
    , perPage       = 30
    , currentPage   = 0
    , totalItems    = Nothing
    , fetch         = f
    }

type Action a
    = ItemsLoaded (Result Http.Error (ApiList a))
    | GoPage Int

totalPages : Model i -> Int
totalPages m =
    Maybe.map (\items -> (items - 1) // m.perPage + 1) m.totalItems |>
    Maybe.withDefault (m.currentPage + 4)

pagination : Model a -> Html (Action a)
pagination m =
    let
        maxPage = totalPages m
        go p =
            Html.li [ HA.classList [ ( "active", m.currentPage == p ) ] ]
                [ Html.a [ HA.href "#", HE.onClick (GoPage p) ] [ Html.text <| toString (p + 1) ] ]
    in
        Html.nav []
            [ Html.ul [ HA.class "pagination" ] <|
                List.map go <| List.range 0 (maxPage - 1)
            ]

refresh : Model a -> Cmd (Action a)
refresh m = m.fetch (m.currentPage * m.perPage, m.perPage) |> Http.send ItemsLoaded

withFetchTask : FetchTask a -> Model a -> (Model a, Cmd (Action a))
withFetchTask ft m =
    let
        m2 = { m | fetch = ft, items = [], currentPage = 0, totalItems = Nothing }
    in
        (m2, refresh m2)

withItemCount : Int -> Model a -> Model a
withItemCount cnt m = { m | totalItems = Just cnt }

updateForResponse : Model a -> ApiList a -> Model a
updateForResponse m al = { m | items = al.items }

update : Action a -> Model a -> (Model a, Cmd (Action a))
update a m = case a of
    ItemsLoaded (Err er)    -> Debug.log "error loading list items" |> \_ -> (m, Cmd.none)
    ItemsLoaded (Ok al)     -> (updateForResponse m al, Cmd.none)
    GoPage p                -> let m2 = { m | currentPage = p } in (m2, refresh m2)
