
module Server (
    ApiList, fetchPersons
    ) where

import Http
import Json.Decode as JD exposing ( (:=) )
import Task exposing ( Task )

import Person exposing ( Person, PersonId, personListDecoder )

apiBaseUrl : String
apiBaseUrl = "http://localhost:8080/api/0.1.0"

type alias ApiList a =
    { offset    : Int
    , count     : Int
    , items     : List a
    }

listDecoder : JD.Decoder a -> JD.Decoder (ApiList a)
listDecoder dec = JD.object3 ApiList
    ( "offset"  := JD.int )
    ( "count"   := JD.int )
    ( "items"   := JD.list dec )

fetchPersons : Task Http.Error (ApiList (PersonId, Person))
fetchPersons = Http.fromJson (listDecoder personListDecoder) (Http.send Http.defaultSettings
    { verb      = "GET"
    , headers   = [("Accept", "application/json")]
    , url       = apiBaseUrl ++ "/person"
    , body      = Http.empty
    })
