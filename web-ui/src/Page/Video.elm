
module Page.Video (
    Model, initialModel, view,
    Action, onMount, update
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Signal exposing ( Address )

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

view : Address Action -> Model -> Html
view aa m = V.view (Signal.forwardTo aa PlayerAction) m.videoModel
    
type Action
    = NoOp
    | PlayerAction V.Action

onMount : FileId -> Model -> (Model, Effects Action)
onMount fid m =
    let
        (v', pfx) = V.setVideo fid m.videoModel

    in
     ( { m | fileId = fid, videoModel = v' }, Effects.map PlayerAction pfx )

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    PlayerAction pa ->
        let
            (p', pfx) = V.update pa m.videoModel
        in
            ( { m | videoModel = p'}, Effects.map PlayerAction pfx )

    _               -> (m, Effects.none)
