
module Types exposing (
    -- * Api Lists
    ApiList, listDecoder,

    -- * Persons
    PersonId, Person, PersonFilter(..), personDecoder, personListDecoder,
    encodePerson,

    -- * Albums
    AlbumId, Album, albumDecoder, WhichAlbums(..),

    -- * Files
    FileId, File, fileDecoder, encodeFileId,
    WhichFiles(..),

    -- * Containers
    Container, containerDecoder,

    -- * Serials
    SerialId, Serial, serialDecoder, serialListDecoder, SerialFilter(..),
    SeasonId, Season, seasonDecoder,
    EpisodeId, Episode, episodeDecoder
    )

import Json.Decode as JD
import Json.Encode as JE

type alias ApiList a =
    { offset    : Int
    , count     : Int
    , items     : List a
    }

listDecoder : JD.Decoder a -> JD.Decoder (ApiList a)
listDecoder dec = JD.map3 ApiList
    ( JD.field "offset" JD.int )
    ( JD.field "count"  JD.int )
    ( JD.field "items"  <| JD.list dec )

------------------------------------------------------------------------------------------------------------------------
-- Persons
------------------------------------------------------------------------------------------------------------------------

type alias PersonId = Int

type alias Person =
    { name      : String
    , portrait  : Maybe FileId
    }

type PersonFilter
    = AllPersons
    | AlbumPersons AlbumId  -- ^ persons in that album

personDecoder : JD.Decoder Person
personDecoder = JD.map2 Person
    ( JD.field "personName"     JD.string )
    ( JD.field "personPortrait" fileIdDecoder |> JD.maybe )

personListDecoder : JD.Decoder (PersonId, Person)
personListDecoder = JD.map2 (,) ( JD.field "personId" JD.int ) personDecoder

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
    { albumId   : AlbumId
    , name      : String
    , poster    : Maybe FileId
    , fileCount : Int
    }

albumDecoder : JD.Decoder Album
albumDecoder = JD.map4 Album
    ( JD.field "albumId"        JD.int )
    ( JD.field "albumName"      JD.string )
    ( JD.field "albumPoster"    fileIdDecoder |> JD.maybe )
    ( JD.field "fileCount"      JD.int )

------------------------------------------------------------------------------------------------------------------------
-- Files
------------------------------------------------------------------------------------------------------------------------

type alias FileId = Int

fileIdDecoder : JD.Decoder FileId
fileIdDecoder = JD.int

encodeFileId : FileId -> JE.Value
encodeFileId = JE.int

type alias File =
    { fileId    : FileId
    , path      : String
    , size      : Int
    , mimeType  : String
    }

fileDecoder : JD.Decoder File
fileDecoder = JD.map4 File
    ( JD.field "fileId"     fileIdDecoder )
    ( JD.field "filePath"   JD.string )
    ( JD.field "fileSize"   JD.int )
    ( JD.field "fileMime"   JD.string )

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
containerDecoder = JD.map2 Container
    ( JD.field "duration"   JD.float )
    ( JD.field "format"     JD.string )

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
serialDecoder = JD.map2 Serial
    ( JD.field "serialName"     JD.string )
    ( JD.field "serialPoster"   fileIdDecoder |> JD.maybe )

serialListDecoder : JD.Decoder (SerialId, Serial)
serialListDecoder = JD.map2 (,) ( JD.field "serialId" serialIdDecoder ) serialDecoder

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
seasonDecoder = JD.map3 Season
    ( JD.field "seasonId"       seasonIdDecoder )
    ( JD.field "seasonSerialId" serialIdDecoder )
    ( JD.field "seasonPoster"   fileIdDecoder |> JD.maybe )

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
episodeDecoder = JD.map5 Episode
    ( JD.field "episodeSerialId"    serialIdDecoder )
    ( JD.field "episodeSeasonId"    seasonIdDecoder )
    ( JD.field "episodeId"          episodeIdDecoder )
    ( JD.field "episodeTitle"       JD.string )
    ( JD.field "episodeFile"        fileIdDecoder |> JD.maybe )
