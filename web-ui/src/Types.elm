
module Types (
    -- * Persons
    PersonId, Person, PersonFilter(..), personDecoder, personListDecoder,

    -- * Albums
    AlbumId, Album,

    -- * Files
    FileId, File, fileDecoder, fileListDecoder,
    WhichFiles(..)
    ) where

import Dict exposing ( Dict )
import Json.Decode as JD exposing ( (:=) )

------------------------------------------------------------------------------------------------------------------------
-- Persons
------------------------------------------------------------------------------------------------------------------------

type alias PersonId = Int

type alias Person =
    { name : String
    }

type PersonFilter
    = AllPersons
    | AlbumPersons AlbumId  -- ^ persons in that album

personDecoder : JD.Decoder Person
personDecoder = JD.object1 Person ( "personName" := JD.string )

personListDecoder : JD.Decoder (PersonId, Person)
personListDecoder = JD.object2 (,) ("personId" := JD.int) personDecoder

------------------------------------------------------------------------------------------------------------------------
-- Albums
------------------------------------------------------------------------------------------------------------------------

type alias AlbumId = Int

type alias Album =
    { name : String }

------------------------------------------------------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------------------------------------------------------

type alias FileId = Int

type alias File =
    { path      : String
    , size      : Int
    , mimeType  : String
    }

fileDecoder : JD.Decoder File
fileDecoder = JD.object3 File
    ( "filePath"    := JD.string )
    ( "fileSize"    := JD.int )
    ( "fileMime"    := JD.string )

fileListDecoder : JD.Decoder (FileId, File)
fileListDecoder = JD.object2 (,) ( "fileId" := JD.int ) fileDecoder

type WhichFiles
    = AllFiles
    | AlbumFiles AlbumId
    | PersonNoAlbum PersonId
