
module Page.SeriesSeasons (
    Model, WithSeasons, initialModel,
    Action, view, update, onMount
    ) where


import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Signal exposing ( Address )
import Task

import File
import Server
import Types exposing (..)

type alias Model =
    { seasonList    : List Season
    , serialInfo    : Maybe Serial
    , serialId      : SerialId
    }

type alias WithSeasons a = { a | pageSeasonsModel : Model }

initialModel : Model
initialModel =
    { seasonList    = []
    , serialInfo    = Nothing
    , serialId      = 0
    }

type Action
    = NoOp
    | GotList (Result Http.Error (ApiList Season))

view : Address Action -> WithSeasons a -> Html
view aa wm =
    let
        m = wm.pageSeasonsModel
        oneSeason s =
            Html.div [ HA.class "col-xs-4 col-md-2" ]
                [ File.thumb (Maybe.withDefault 1 s.seasonPoster)
                , Html.text <| toString s.seasonId
                ]
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-header" ] [ Html.text "Seasons for ..." ]
            , Html.div [ HA.class "row" ] <|
                List.map oneSeason m.seasonList
            ]

onMount : WithSeasons a -> (WithSeasons a, Effects Action)
onMount wm = ( wm, Server.fetchSeasons wm.pageSeasonsModel.serialId |> Task.map GotList |> Effects.task )

update : Action -> WithSeasons a -> (WithSeasons a, Effects Action)
update a wm =
    let
        m           = wm.pageSeasonsModel
        noFx x      = ( x, Effects.none )
        (m', fx)    = case a of
            NoOp                -> noFx m
            GotList (Err er)    -> Debug.log "fetching seasons failed" er |> \_ -> noFx m
            GotList (Ok al)     -> { m | seasonList = al.items } |> noFx
    in
        ( { wm | pageSeasonsModel = m'}, fx )
