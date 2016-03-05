
module Page.Video (
    Model, initialModel, view,
    Action, onMount, update
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Signal exposing ( Address )

import Types exposing (..)

type alias Model =
    { fileId : FileId
    }

initialModel : Model
initialModel =
    { fileId = 0
    }

view : Address Action -> Model -> Html
view aa m = Html.text "Video hier"

type Action =
    NoOp

onMount : FileId -> Model -> (Model, Effects Action)
onMount fid m = ( { m | fileId = fid }, Effects.none )

update : Action -> Model -> (Model, Effects Action)
update a m = (m, Effects.none)
