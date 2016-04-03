
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
import Route exposing ( clickRoute )

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

seasonName : SeasonId -> String
seasonName sid = case sid of
    0   -> "Specials"
    n   -> "Season " ++ toString n

view : Address Action -> WithSeasons a -> Html
view aa wm =
    let
        m = wm.pageSeasonsModel
        oneSeason s =
            Html.div [ HA.class "col-xs-4 col-md-2" ]
                [ Html.a ( HA.class "thumbnail" :: (clickRoute <| Route.SeriesEpisodes s.seasonSerial s.seasonId))
                    [ File.thumb File.Poster (Maybe.withDefault 0 s.seasonPoster)
                    , Html.span [ HA.class "item-name" ]
                        [ Html.text <| seasonName s.seasonId ]
                    ]
                ]
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-header" ] [ Html.text "Seasons for ..." ]
            , Html.div [ HA.class "row" ] <|
                List.map oneSeason m.seasonList
            ]

onMount : SerialId -> WithSeasons a -> (WithSeasons a, Effects Action)
onMount sid wm =
    let
        m = wm.pageSeasonsModel
        l' = if sid == m.serialId then m.seasonList else []
        m' = { m | serialId = sid, seasonList = l' }
        fx = Server.fetchSeasons sid |> Task.map GotList |> Effects.task
    in
        ( { wm | pageSeasonsModel = m' }, fx)

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
