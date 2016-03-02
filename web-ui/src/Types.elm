
module Types (
    -- * Persons
    PersonId, Person, WithPersons
    ) where

import Dict exposing ( Dict )

type alias PersonId = Int

type alias Person =
    { name : String
    }

type alias WithPersons a =
    { a
    | persons   : Dict PersonId Person
    }
