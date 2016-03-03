
module Album (
    albumDecoder, albumListDecoder
    ) where

import Json.Decode as JD exposing ( (:=) )

import Types exposing ( AlbumId, Album )

albumDecoder : JD.Decoder Album
albumDecoder = JD.object1 Album
    ( "albumName" := JD.string )

albumListDecoder : JD.Decoder (AlbumId, Album)
albumListDecoder = JD.object2 (,) ("albumId" := JD.int) albumDecoder
