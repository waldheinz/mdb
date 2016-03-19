
module Page.Series (
    Model, WithSeries, initialModel,
    Action, view, update, onMount
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Signal exposing ( Address )
import Task

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

fileThumb : FileId -> Html
fileThumb fid =
    Html.div [ HA.class "file-thumb-container" ]
        [ Html.div [ HA.class "file-thumb", HA.style [("background-image", "url(" ++ Server.fileThumbUrl fid ++ ")") ] ]
            [  ]
        ]

view : Address Action -> WithSeries a -> Html
view aa wm =
    let
        m = wm.pageSeriesModel
        oneSeries (sid, s) =
            Html.div [ HA.class "col-md-2" ]
                [ fileThumb (Maybe.withDefault 1 s.serialPoster)
                , Html.text s.serialName
                ]
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-header" ] [ Html.text "Series" ]
            , Html.div [ HA.class "row" ] <|
                List.map oneSeries m.serialList
            ]

onMount : WithSeries a -> (WithSeries a, Effects Action)
onMount wm = ( wm, Server.fetchSerials AllSerials |> Task.map GotList |> Effects.task )

update : Action -> WithSeries a -> (WithSeries a, Effects Action)
update a wm =
    let
        m           = wm.pageSeriesModel
        noFx x      = ( x, Effects.none )
        (m', fx)    = case a of
            NoOp                -> noFx m
            GotList (Err er)    -> Debug.log "fetching series failed" er |> \_ -> noFx m
            GotList (Ok al)     -> { m | serialList = al.items } |> noFx
    in
        ( { wm | pageSeriesModel = m'}, fx )
