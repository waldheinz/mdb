
module Person (
    -- * I/O
    personDecoder, personListDecoder,

    -- * Views
    ListAction, viewList
    ) where

import Dict
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD exposing ( (:=) )
import Signal exposing ( Address )

import Route exposing ( clickRoute )
import Types exposing ( PersonId, Person, WithPersons )

personDecoder : JD.Decoder Person
personDecoder = JD.object1 Person ( "personName" := JD.string )

personListDecoder : JD.Decoder (PersonId, Person)
personListDecoder = JD.object2 (,) ("personId" := JD.int) personDecoder

type ListAction
    = PersonSelected PersonId

viewList : Address ListAction -> WithPersons a -> Html
viewList aa m =
    let
        onePerson (pid, p) =
            Html.div [ HA.class "col-xs-2" ]
                [ Html.a (HA.class "thumbnail" :: clickRoute (Route.Person pid))
                    [ Html.text p.name ]
                ]
    in
        Dict.toList m.persons |> List.map onePerson |> Html.div [ HA.class "row" ]
