
module Page.Person (
    Model, Action, view, update
    ) where

import Html exposing ( Html )
import Signal

import Types exposing ( PersonId )

type alias Model = { personId : PersonId }

type Action = NoOp

view : Signal.Address Action -> Model -> Html
view aa m = Html.div [] [ Html.text "Person hier" ]

update : Action -> Model -> Model
update a m = m
