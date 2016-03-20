
module Types (
    -- * Api Lists
    ApiList, listDecoder,

    -- * Persons
    PersonId, Person, PersonFilter(..), personDecoder, personListDecoder,
    encodePerson,

    -- * Albums
    AlbumId, Album, WhichAlbums(..), albumListDecoder,

    -- * Files
    FileId, File, fileDecoder, fileListDecoder,
    WhichFiles(..),

    -- * Containers
    Container, containerDecoder,

    -- * Serials
    SerialId, Serial, serialDecoder, serialListDecoder, SerialFilter(..),
    SeasonId, Season, seasonDecoder,
    EpisodeId, Episode, episodeDecoder
    ) where

import Json.Decode as JD exposing ( (:=) )
import Json.Encode as JE

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
personListDecoder = JD.object2 (,) ( "personId" := JD.int ) personDecoder

encodePerson : PersonId -> Person -> JE.Value
encodePerson pid p = JE.object
    [ ( "personId"      , JE.int pid )
    , ( "personName"    , JE.string p.name )
    ]

------------------------------------------------------------------------------------------------------------------------
-- Albums
------------------------------------------------------------------------------------------------------------------------

type WhichAlbums
    = AllAlbums
    | PersonAlbums PersonId

type alias AlbumId = Int

type alias Album =
    { name          : String
    , albumPoster   : Maybe FileId
    }

albumDecoder : JD.Decoder Album
albumDecoder = JD.object2 Album
    ( "albumName"   := JD.string )
    ( "albumPoster" := fileIdDecoder |> JD.maybe )

albumListDecoder : JD.Decoder (AlbumId, Album)
albumListDecoder = JD.object2 (,) ("albumId" := JD.int) albumDecoder

------------------------------------------------------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------------------------------------------------------

type alias FileId = Int

fileIdDecoder : JD.Decoder FileId
fileIdDecoder = JD.int

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
fileListDecoder = JD.object2 (,) ( "fileId" := fileIdDecoder ) fileDecoder

type WhichFiles
    = AllFiles
    | AlbumFiles AlbumId
    | PersonNoAlbum PersonId

------------------------------------------------------------------------------------------------------------------------
-- Containers
------------------------------------------------------------------------------------------------------------------------

type alias Container =
    { duration  : Float
    , format    : String
    }

containerDecoder : JD.Decoder Container
containerDecoder = JD.object2 Container
    ( "duration"    := JD.float )
    ( "format"      := JD.string )

------------------------------------------------------------------------------------------------------------------------
-- Serials
------------------------------------------------------------------------------------------------------------------------

type alias SerialId = Int

serialIdDecoder : JD.Decoder SerialId
serialIdDecoder = JD.int

type alias Serial =
    { serialName    : String
    , serialPoster  : Maybe FileId
    }

serialDecoder : JD.Decoder Serial
serialDecoder = JD.object2 Serial
    ( "serialName"      := JD.string )
    ( "serialPoster"    := fileIdDecoder |> JD.maybe )

serialListDecoder : JD.Decoder (SerialId, Serial)
serialListDecoder = JD.object2 (,) ( "serialId" := serialIdDecoder ) serialDecoder

type SerialFilter = AllSerials

-- seasons

type alias SeasonId = Int

seasonIdDecoder : JD.Decoder SeasonId
seasonIdDecoder = JD.int

type alias Season =
    { seasonId      : SeasonId
    , seasonSerial  : SerialId
    , seasonPoster  : Maybe FileId
    }

seasonDecoder : JD.Decoder Season
seasonDecoder = JD.object3 Season
    ( "seasonId"        := seasonIdDecoder )
    ( "seasonSerialId"  := serialIdDecoder )
    ( "seasonPoster"    := fileIdDecoder |> JD.maybe )

-- episodes

type alias EpisodeId = Int

episodeIdDecoder : JD.Decoder EpisodeId
episodeIdDecoder = JD.int

type alias Episode =
    { serialId  : SerialId
    , seasonId  : SeasonId
    , episodeId : EpisodeId
    , title     : String
    , fileId    : Maybe FileId
    }

episodeDecoder : JD.Decoder Episode
episodeDecoder = JD.object5 Episode
    ( "episodeSerialId" := serialIdDecoder )
    ( "episodeSeasonId" := seasonIdDecoder )
    ( "episodeId"       := episodeIdDecoder )
    ( "episodeTitle"    := JD.string )
    ( "episodeFile"     := fileIdDecoder |> JD.maybe )
