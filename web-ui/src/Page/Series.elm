
module Page.Series exposing (
    Model, WithSeries, initialModel,
    Action, view, update, onMount
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Task

import File
import Route exposing ( Route(..), clickRoute )
import Server
import Types exposing (..)

type alias Model =
    { serialList    : List (SerialId, Serial)
    }

type alias WithSeries a = { a | pageSeriesModel : Model }

initialModel : Model
initialModel =
    { serialList    = []
    }

type Action
    = NoOp
    | GotList (Result Http.Error (ApiList (SerialId, Serial)))

view : WithSeries a -> Html Action
view wm =
    let
        m = wm.pageSeriesModel
        oneSeries (sid, s) =
            Html.div [ HA.class "col-xs-3 col-md-2" ]
                [ Html.a ( HA.class "thumbnail" :: (clickRoute <| SeriesSeasons sid))
                    [ File.thumb File.Poster (Maybe.withDefault 0 s.serialPoster)
                    , Html.span [ HA.class "item-name" ]
                        [ Html.text s.serialName ]
                    ]
                ]
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-header" ] [ Html.text "Series" ]
            , Html.div [ HA.class "row" ] <|
                List.map oneSeries m.serialList
            ]

onMount : WithSeries a -> (WithSeries a, Cmd Action)
onMount wm = ( wm, Server.fetchSerials AllSerials |> Task.perform (Err >> GotList) (Ok >> GotList) )

update : Action -> WithSeries a -> (WithSeries a, Cmd Action)
update a wm =
    let
        m           = wm.pageSeriesModel
        noFx x      = ( x, Cmd.none )
        (m', fx)    = case a of
            NoOp                -> noFx m
            GotList (Err er)    -> Debug.log "fetching series failed" er |> \_ -> noFx m
            GotList (Ok al)     -> { m | serialList = al.items } |> noFx
    in
        ( { wm | pageSeriesModel = m'}, fx )
