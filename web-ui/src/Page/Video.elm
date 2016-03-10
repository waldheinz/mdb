
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
import VideoPlayer as V

type alias Model =
    { fileId        : FileId
    , videoInfo     : Maybe Video
    , videoModel    : V.Model
    }

initialModel : Model
initialModel =
    { fileId        = 0
    , videoInfo     = Nothing
    , videoModel    = V.initialModel "page-video"
    }

view : Address Action -> Model -> Html
view aa m =
        Html.div [ HA.class "embed-responsive embed-responsive-16by9" ]
            [ V.view m.videoModel ]

type Action
    = NoOp
    | FetchedVideoInfo (Result Http.Error Video)

onMount : FileId -> Model -> (Model, Effects Action)
onMount fid m =
    let
        v'  = if fid == m.fileId then m.videoModel else m.videoModel |> V.setVideo fid
        vi' = if fid == m.fileId then m.videoInfo else Nothing
        fx  = Server.fetchVideoForFile fid |> Task.toResult |> Task.map FetchedVideoInfo |> Effects.task
    in
     ( { m | fileId = fid, videoInfo = vi', videoModel = v' }, fx )

update : Action -> Model -> (Model, Effects Action)
update a m = (m, Effects.none)
