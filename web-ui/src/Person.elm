
module Person (
    -- * I/O
    personDecoder, personListDecoder,

    -- * Views
    viewList
    ) where

import Dict
import Html exposing ( Html )
import Html.Attributes as HA
import Json.Decode as JD exposing ( (:=) )

import Types exposing ( PersonId, Person, WithPersons )

personDecoder : JD.Decoder Person
personDecoder = JD.object1 Person ( "personName" := JD.string )

personListDecoder : JD.Decoder (PersonId, Person)
personListDecoder = JD.object2 (,) ("personId" := JD.int) personDecoder

viewList : WithPersons a -> Html
viewList m =
    let
        onePerson (pid, p) =
            Html.div [ HA.class "col-xs-2" ]
                [ Html.a [ HA.class "thumbnail" ]
                    [ Html.text p.name ]
                ]
    in
        Dict.toList m.persons |> List.map onePerson |> Html.div [ HA.class "row" ]
