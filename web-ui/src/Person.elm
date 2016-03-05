
module Person (
    -- * List
    ListModel, initialListModel, viewList,
    ListAction, setListFilter, updateListModel
    ) where

import Dict
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD exposing ( (:=) )
import Signal exposing ( Address )
import Task

import Route exposing ( clickRoute )
import Server exposing ( ApiList )
import Types exposing ( .. )

type alias ListModel =
    { persons       : List (PersonId, Person)
    , personFilter  : PersonFilter
    }

initialListModel : ListModel
initialListModel =
    { persons       = []
    , personFilter  = AllPersons
    }

type ListAction
    = PersonSelected PersonId
    | PersonsLoaded (Result Http.Error (ApiList (PersonId, Person)))

setListFilter : PersonFilter -> ListModel -> (ListModel, Effects ListAction)
setListFilter which m =
    let
        fs' = if which == m.personFilter then m.persons else []
    in
        ( { m | personFilter = which, persons = fs' }
        , Server.fetchPersons which |> Task.toResult |> Task.map PersonsLoaded |> Effects.task
        )


viewList : Address ListAction -> ListModel -> Html
viewList aa m =
    let
        onePerson (pid, p) =
            Html.div [ HA.class "col-xs-2" ]
                [ Html.a (HA.class "thumbnail" :: clickRoute (Route.Person pid))
                    [ Html.text p.name ]
                ]
    in
        List.map onePerson m.persons |> Html.div [ HA.class "row" ]

updateListModel : ListAction -> ListModel -> ListModel
updateListModel a m = case a of
    PersonSelected _ -> m -- handled externally
    PersonsLoaded (Ok pl)   -> { m | persons = pl.items }
    PersonsLoaded (Err ex)  -> Debug.log "loading persons failed" ex |> \_ -> m
