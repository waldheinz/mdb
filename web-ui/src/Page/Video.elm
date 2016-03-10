
module Page.Video (
    Model, initialModel, view,
    Action, onMount, update
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
    { fileId    : FileId
    , videoInfo : Maybe Video
    }

initialModel : Model
initialModel =
    { fileId    = 0
    , videoInfo = Nothing
    }

view : Address Action -> Model -> Html
view aa m =
    let
        player =
            Html.video
                [ HA.src <| Server.videoStreamUrl m.fileId
                , HA.controls True
                , HA.type' "video/webm"
                , HA.poster <| Server.videoFrameUrl m.fileId 200
                ]
                [ Html.text "Kein Video hier?" ]
    in
        Html.div [ HA.class "embed-responsive embed-responsive-16by9" ]
            [ player ]


type Action
    = NoOp
    | FetchedVideoInfo (Result Http.Error Video)

onMount : FileId -> Model -> (Model, Effects Action)
onMount fid m =
    let
        v'  = if fid == m.fileId then m.videoInfo else Nothing
        fx  = Server.fetchVideoForFile fid |> Task.toResult |> Task.map FetchedVideoInfo |> Effects.task
    in
     ( { m | fileId = fid, videoInfo = v' }, fx )

update : Action -> Model -> (Model, Effects Action)
update a m = (m, Effects.none)
