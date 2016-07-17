
module Page.Video exposing (
    Model, initialModel, view,
    Action(PlayerAction), onMount, update
    )

import Html exposing ( Html )
import Html.App
import Html.Attributes as HA

import Types exposing (..)
import VideoPlayer as V

type alias Model =
    { fileId        : FileId
    , videoModel    : V.Model
    }

initialModel : Model
initialModel =
    { fileId        = 0
    , videoModel    = V.initialModel "page-video"
    }

view : Model -> Html Action
view m =
    Html.div [ HA.class "container" ]
        [ Html.App.map PlayerAction (V.view m.videoModel)
        ]

type Action
    = NoOp
    | PlayerAction V.Action

onMount : FileId -> Model -> (Model, Cmd Action)
onMount fid m =
    let
        (v', pfx) = V.setVideo fid m.videoModel

    in
     ( { m | fileId = fid, videoModel = v' }, Cmd.map PlayerAction pfx )

update : Action -> Model -> (Model, Cmd Action)
update a m = case a of
    PlayerAction pa ->
        let
            (p', pfx) = V.update pa m.videoModel
        in
            ( { m | videoModel = p'}, Cmd.map PlayerAction pfx )

    _               -> (m, Cmd.none)
