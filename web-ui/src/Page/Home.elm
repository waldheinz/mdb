
module Page.Home (
    Action, view, update
    ) where

import Html exposing ( Html )
import Html.Attributes as HA

import Person
import Types exposing ( WithPersons )

type alias Model a = WithPersons a

type Action
    = NoOp
    | PersonListAction Person.ListAction

view : Signal.Address Action -> Model a -> Html
view aa m =
    Html.div []
        [ Html.h1 [ HA.class "page-lead" ] [ Html.text "Persons" ]
        , Person.viewList (Signal.forwardTo aa PersonListAction) m
        ]

update : Action -> Model a -> Model a
update a m = m
