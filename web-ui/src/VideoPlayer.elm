
module VideoPlayer (
    Model, initialModel, setVideo, view, controls,
    Action(Play), update
    ) where

import Json.Decode as JD
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Signal exposing ( Address )
import Task exposing ( Task )

import Server
import Types exposing (..)
import Utils exposing ( onClick' )
import Native.VideoPlayer

type PlayState = Playing | Paused

type alias Model =
    { videoBaseUrl  : String
    , fileId        : FileId
    , playTime      : Float
    , playState     : PlayState
    , playerId      : String
    }

initialModel : String -> Model
initialModel playerId =
    { videoBaseUrl  = ""
    , fileId        = 0
    , playTime      = 0
    , playState     = Paused
    , playerId      = playerId
    }

setVideo : FileId -> Model -> Model
setVideo fid m = { m | fileId = fid, playTime = 0, videoBaseUrl = Server.videoStreamUrl fid }

type Action
    = NoOp
    | PlayStateChanged PlayState
    | Play
    | Pause

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    Play                -> (m, setPlay m True |> Effects.task)
    Pause               -> (m, setPlay m False |> Effects.task)
    PlayStateChanged s  -> ( { m | playState = s }, Effects.none )
    _                   -> (m, Effects.none)

setPlay : Model -> Bool -> Task Effects.Never Action
setPlay m play = Native.VideoPlayer.setPlay m play

view : Address Action -> Model -> Html
view aa m =
    Html.video
        [ HA.type' "video/webm"
        , HA.id m.playerId
        , HA.poster <| Server.videoFrameUrl m.fileId 200
        , HE.on "playing" (JD.succeed ()) (\() -> Signal.message aa (PlayStateChanged Playing))
        , HE.on "pause" (JD.succeed ()) (\() -> Signal.message aa (PlayStateChanged Paused))
        ]
        [ Html.text "Kein Video hier?" ]

controls : Address Action -> Model -> Html
controls aa m =
    let
        playClick = onClick' aa (if m.playState == Paused then Play else Pause)
    in
        Html.button [ playClick ] [ Html.text "play" ]
