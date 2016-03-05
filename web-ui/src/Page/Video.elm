
module Page.Video (
    Model, initialModel, view,
    Action, onMount, update
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Signal exposing ( Address )

import Server
import Types exposing (..)

type alias Model =
    { fileId : FileId
    }

initialModel : Model
initialModel =
    { fileId = 0
    }

view : Address Action -> Model -> Html
view aa m =
    Html.video
        [ HA.src <| Server.videoStreamUrl m.fileId
        , HA.controls True
        , HA.type' "video/webm"
        , HA.poster <| Server.videoFrameUrl m.fileId 200
        ]
        [ Html.text "Kein Video hier?" ]


type Action =
    NoOp

onMount : FileId -> Model -> (Model, Effects Action)
onMount fid m = ( { m | fileId = fid }, Effects.none )

update : Action -> Model -> (Model, Effects Action)
update a m = (m, Effects.none)
