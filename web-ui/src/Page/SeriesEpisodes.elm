
module Page.SeriesEpisodes exposing (
    Model, WithEpisodes, initialModel,
    Action, view, update, onMount
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Task

import File
import Route exposing ( clickRoute )
import Server
import Types exposing (..)

type alias Model =
    { episodeList   : List Episode
    , seasonInfo    : Maybe Season
    , seasonId      : SeasonId
    , serialId      : SerialId
    }

type alias WithEpisodes a = { a | pageEpisodesModel : Model }

initialModel : Model
initialModel =
    { episodeList   = []
    , seasonInfo    = Nothing
    , seasonId      = 0
    , serialId      = 0
    }

type Action
    = NoOp
    | GotList (Result Http.Error (ApiList Episode))

view : Address Action -> WithEpisodes a -> Html
view aa wm =
    let
        m = wm.pageEpisodesModel
        oneEpisode ep =
            let
                onClick = Maybe.map (Route.Video >> clickRoute) ep.fileId |>  Maybe.withDefault []
            in
                Html.div [ HA.class "col-xs-4 col-md-2" ]
                    [ Html.a (HA.class "thumbnail" :: onClick )
                        [ File.thumb File.Movie (Maybe.withDefault 0 ep.fileId)
                        , Html.span [ HA.class "item-name" ]
                            [ Html.text ep.title ]
                        ]
                    ]
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-header" ] [ Html.text "Episodes for ..." ]
            , Html.div [ HA.class "row" ] <|
                List.map oneEpisode m.episodeList
            ]

onMount : SerialId -> SeasonId -> WithEpisodes a -> (WithEpisodes a, Effects Action)
onMount serId seaId wm =
    let
        m = wm.pageEpisodesModel
        l' = if (serId, seaId) == (m.serialId, m.seasonId) then m.episodeList else []
        m' = { m | serialId = serId, seasonId = seaId, episodeList = l' }
        fx = Server.fetchEpisodes serId seaId |> Task.map GotList |> Effects.task
    in
        ( { wm | pageEpisodesModel = m' }, fx)

update : Action -> WithEpisodes a -> (WithEpisodes a, Effects Action)
update a wm =
    let
        m           = wm.pageEpisodesModel
        noFx x      = ( x, Effects.none )
        (m', fx)    = case a of
            NoOp                -> noFx m
            GotList (Err er)    -> Debug.log "fetching episodes failed" er |> \_ -> noFx m
            GotList (Ok al)     -> { m | episodeList = al.items } |> noFx
    in
        ( { wm | pageEpisodesModel = m'}, fx )
