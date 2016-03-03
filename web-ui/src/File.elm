
module File (
    fileDecoder, fileListDecoder
    ) where

import Json.Decode as JD exposing ( (:=) )

import Types exposing ( FileId, File )

fileDecoder : JD.Decoder File
fileDecoder = JD.object3 File
    ( "filePath"    := JD.string )
    ( "fileSize"    := JD.int )
    ( "fileMime"    := JD.string )

fileListDecoder : JD.Decoder (FileId, File)
fileListDecoder = JD.object2 (,) ( "fileId" := JD.int ) fileDecoder
