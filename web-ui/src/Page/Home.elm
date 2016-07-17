
module Page.Home exposing (
    Model, initialModel, onMount, Action, view, update
    )

import Html exposing ( Html )
import Html.Attributes as HA

import Person
import Types exposing ( .. )

type alias Model =
    { persons   : Person.ListModel
    }

initialModel : Model
initialModel =
    { persons   = Person.initialListModel
    }

type Action
--    = NoOp
    = PersonListAction Person.ListAction

onMount : Model -> (Model, Effects Action)
onMount m =
    let
        (pl', plfx) = Person.setListFilter AllPersons m.persons
    in
        ({ m | persons = pl' }, Effects.map PersonListAction plfx)

view : Signal.Address Action -> Model -> Html
view aa m =
    Html.div [ HA.class "container" ]
        [ Html.h1 [ HA.class "page-lead" ] [ Html.text "Persons" ]
        , Person.viewList (Signal.forwardTo aa PersonListAction) m.persons
        ]

update : Action -> Model -> Model
update a m = case a of
    PersonListAction pla -> { m | persons = Person.updateListModel pla m.persons }
