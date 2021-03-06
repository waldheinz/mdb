
module Person exposing (
    -- * List
    ListModel, initialListModel, listEmpty, viewList,
    ListAction, setListFilter, updateListModel,

    -- * Editing
    updatePerson
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Task

import File
import Route exposing ( clickRoute )
import Server
import Types exposing ( .. )

------------------------------------------------------------------------------------------------------------------------
-- List
------------------------------------------------------------------------------------------------------------------------

type alias ListModel =
    { persons       : List (PersonId, Person)
    , personFilter  : PersonFilter
    }

initialListModel : ListModel
initialListModel =
    { persons       = []
    , personFilter  = AllPersons
    }

listEmpty : ListModel -> Bool
listEmpty m = List.isEmpty m.persons

type ListAction
    = PersonSelected PersonId
    | PersonsLoaded (Result Http.Error (ApiList (PersonId, Person)))

setListFilter : PersonFilter -> ListModel -> (ListModel, Cmd ListAction)
setListFilter which m =
    let
        fs' = if which == m.personFilter then m.persons else []
    in
        ( { m | personFilter = which, persons = fs' }
        , Server.fetchPersons which |> Task.perform (Err >> PersonsLoaded) (Ok >> PersonsLoaded)
        )

viewList : ListModel -> Html ListAction
viewList m =
    let
        onePerson (pid, p) =
            Html.div [ HA.class "col-xs-3 col-md-2" ]
                [ Html.a (HA.class "thumbnail" :: clickRoute (Route.Person pid))
                    [ Maybe.withDefault 0 p.portrait |> File.thumb File.Poster
                    , Html.span [ HA.class "item-name" ] [ Html.text p.name ]
                    ]
                ]
    in
        List.map onePerson m.persons |> Html.div [ HA.class "row" ]

updateListModel : ListAction -> ListModel -> ListModel
updateListModel a m = case a of
    PersonSelected _ -> m -- handled externally
    PersonsLoaded (Ok pl)   -> { m | persons = pl.items }
    PersonsLoaded (Err ex)  -> Debug.log "loading persons failed" ex |> \_ -> m

------------------------------------------------------------------------------------------------------------------------
-- Edit
------------------------------------------------------------------------------------------------------------------------

updatePerson : (Person -> Person) -> PersonId -> Person -> (Person, Cmd ())
updatePerson f pid p =
    let
        p' = f p
    in
        (p', Server.putPerson pid p' |> Task.perform (\_ -> ()) (\_ -> ()))
