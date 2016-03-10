
module VideoPlayer (
    Model, initialModel, setVideo,
    view
    ) where

import Html exposing ( Html )
import Html.Attributes as HA

import Server
import Types exposing (..)

type PlayState = Playing | Paused

type alias Model =
    { videoSrc      : String
    , fileId        : FileId
    , playTime      : Float
    , playState     : PlayState
    , playerId      : String
    }

initialModel : String -> Model
initialModel playerId =
    { videoSrc      = ""
    , fileId        = 0
    , playTime      = 0
    , playState     = Paused
    , playerId      = playerId
    }

setVideo : FileId -> Model -> Model
setVideo fid m = { m | fileId = fid, playTime = 0, videoSrc = Server.videoStreamUrl fid }

type Action
    = PlayStateChanged PlayState
    | Play

play : Model -> Model
play m = Native.VideoPlayer.startPlay

view : Model -> Html
view m =
    Html.video
        [ HA.type' "video/webm"
        , HA.id m.playerId
        , HA.poster <| Server.videoFrameUrl m.fileId 200
        ]
        [ Html.text "Kein Video hier?" ]
