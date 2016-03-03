
module Types (
    -- * Persons
    PersonId, Person, WithPersons,

    -- * Albums
    AlbumId, Album
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

type alias AlbumId = Int

type alias Album =
    { name : String }
