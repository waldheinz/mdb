
module Page.Person (
    Model, Action, view, update, onMount, initialModel
    ) where

import Dict exposing ( Dict )
import Effects exposing ( Effects )
import Html exposing ( Html )
import Signal

import Types exposing ( Album, AlbumId, PersonId )

type alias Model =
    { personId  : PersonId
    , albums    : Dict AlbumId Album
    }

initialModel : Model
initialModel =
    { personId  = 0
    , albums    = Dict.empty
    }

onMount : Effects Action
onMount = Effects.none

type Action = NoOp

view : Signal.Address Action -> Model -> Html
view aa m = Html.div [] [ Html.text "Person hier" ]

update : Action -> Model -> Model
update a m = m
