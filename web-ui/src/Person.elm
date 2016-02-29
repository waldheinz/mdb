
module Person (
    PersonId, Person, personDecoder, personListDecoder
    ) where

import Json.Decode as JD exposing ( (:=) )

type alias PersonId = Int

type alias Person =
    { name : String
    }

personDecoder : JD.Decoder Person
personDecoder = JD.object1 Person ( "personName" := JD.string )

personListDecoder : JD.Decoder (PersonId, Person)
personListDecoder = JD.object2 (,) ("personId" := JD.int) personDecoder
