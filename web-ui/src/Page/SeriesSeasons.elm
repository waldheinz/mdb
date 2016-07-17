
module Page.SeriesSeasons exposing (
    Model, WithSeasons, initialModel,
    Action, view, update, onMount
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Task

import File
import Server
import Types exposing (..)
import Route exposing ( clickRoute )

type alias Model =
    { seasonList        : List Season
    , serialDescription : String
    , serialInfo        : Maybe Serial
    , serialId          : SerialId
    }

type alias WithSeasons a = { a | pageSeasonsModel : Model }

initialModel : Model
initialModel =
    { seasonList        = []
    , serialDescription = ""
    , serialInfo        = Nothing
    , serialId          = 0
    }

type Action
    = NoOp
    | GotList (Result Http.Error (ApiList Season))
    | GotInfo (Result Http.Error Serial)
    | GotDesc (Result Http.Error String)

seasonName : SeasonId -> String
seasonName sid = case sid of
    0   -> "Specials"
    n   -> "Season " ++ toString n

headerText : Model -> String
headerText m = case m.serialInfo of
    Nothing -> ""
    Just i  -> i.serialName

serialPoster : Model -> Html never
serialPoster m = case m.serialInfo of
    Nothing -> Html.text "(poster)"
    Just i  -> case i.serialPoster of
        Nothing     ->  Html.text "(adasdasd)"
        Just fid    -> File.thumb File.Poster fid

view : WithSeasons a -> Html Action
view wm =
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
            [ Html.div [ HA.class "row" ]
                [ Html.div [ HA.class "col-xs-3" ]
                    [  serialPoster m ]
                , Html.div [ HA.class "col-xs-9" ]
                    [ Html.h1 [ HA.class "page-header" ] [ Html.text <| headerText m ]
                    , Html.p [] [ Html.text m.serialDescription ]
                    ]
                ]
            , Html.div [ HA.class "row" ] <|
                List.map oneSeason m.seasonList
            ]

onMount : SerialId -> WithSeasons a -> (WithSeasons a, Cmd Action)
onMount sid wm =
    let
        m               = wm.pageSeasonsModel
        (l', i', d')    = if sid == m.serialId
            then ( m.seasonList, m.serialInfo, m.serialDescription )
            else ( [], Nothing, "" )

        m'              = { m | serialId = sid, seasonList = l', serialInfo = i', serialDescription = d' }
        fetchSeasons    = Server.fetchSeasons sid |> Task.perform (Err >> GotList) (Ok >> GotList)
        fetchInfo       = Server.fetchSerialInfo sid |> Task.perform (Err >> GotInfo) (Ok >> GotInfo)
        fetchDesc       = Server.fetchSerialDescription sid |> Task.perform (Err >> GotDesc) (Ok >> GotDesc)
    in
        ( { wm | pageSeasonsModel = m' }, Cmd.batch [ fetchSeasons, fetchInfo, fetchDesc ] )

update : Action -> WithSeasons a -> (WithSeasons a, Cmd Action)
update a wm =
    let
        m           = wm.pageSeasonsModel
        noFx x      = ( x, Cmd.none )
        (m', fx)    = case a of
            NoOp                -> noFx m
            GotDesc (Err er)    -> Debug.log "fetching description failed" er |> \_ -> noFx m
            GotDesc (Ok d)      -> { m | serialDescription = d } |> noFx
            GotList (Err er)    -> Debug.log "fetching seasons failed" er |> \_ -> noFx m
            GotList (Ok al)     -> { m | seasonList = al.items } |> noFx
            GotInfo (Err er)    -> Debug.log "fetching serial info faild" er |> \_ -> noFx m
            GotInfo (Ok i)      -> { m | serialInfo = Just i } |> noFx
    in
        ( { wm | pageSeasonsModel = m' }, fx )
