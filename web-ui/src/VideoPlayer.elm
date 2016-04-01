
module VideoPlayer (
    Model, initialModel, setVideo, view,
    Action, update, input
    ) where

import Json.Decode as JD
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Mouse
import Signal exposing ( Address, Signal )
import String
import Task exposing ( Task )
import Time

import Server
import Types exposing (..)
import Utils exposing ( onClick' )
import Native.VideoPlayer

type PlayState = Playing | Paused | Seeking

type alias Model =
    { videoBaseUrl  : String
    , fileId        : FileId
    , playTime      : Float
    , playStartTime : Float
    , playState     : PlayState
    , playerId      : String
    , videoInfo     : Maybe Container
    , mouseMoved    : Bool
    , overControls  : Bool              -- ^ does the mouse cursor hover over the control panel?
    , doSeek        : Bool              -- ^ if there is a seek operation pending (so we have to set src again)
    }

initialModel : String -> Model
initialModel playerId =
    { videoBaseUrl  = ""
    , fileId        = 0
    , playTime      = 0
    , playStartTime = 0
    , playState     = Paused
    , playerId      = playerId
    , videoInfo     = Nothing
    , mouseMoved    = True
    , overControls  = False
    , doSeek        = True
    }

input : Signal Action
input = Time.since (3 * Time.second) (Mouse.position) |> Signal.map MouseMoved

setVideo : FileId -> Model -> (Model, Effects Action)
setVideo fid m =
    let
        m' = if fid == m.fileId
                then m
                else
                    { m
                    | fileId        = fid
                    , playTime      = 0
                    , playStartTime = 0
                    , videoBaseUrl  = Server.videoBaseUrl fid
                    , videoInfo     = Nothing
                    }
        fx = Server.fetchContainerForFile fid |> Task.toResult |> Task.map FetchedVideoInfo |> Effects.task
    in
        ( m', fx )

type Action
    = NoOp
    | PlayStateChanged PlayState
    | PlayTimeChanged Float
    | Play
    | Pause
    | FetchedVideoInfo (Result Http.Error Container)
    | SeekTo Float
    | SeekDone
    | MouseMoved Bool
    | OverControls Bool
    | GoFullscreen

currentTime : Model -> Float
currentTime m = case m.playState of
    Paused  -> m.playStartTime
    Playing -> m.playStartTime + m.playTime
    Seeking -> m.playStartTime

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    NoOp                -> ( m, Effects.none )
    Play                -> ( { m | doSeek = False }, setPlay m True |> Effects.task )
    Pause               -> ( { m | playStartTime = currentTime m }, setPlay m False |> Effects.task )
    PlayStateChanged s  -> ( { m | playState = s }, Effects.none )
    PlayTimeChanged t   -> ( { m | playTime = t }, Effects.none )
    FetchedVideoInfo (Ok v) -> ( { m | videoInfo = Just v }, Effects.none )
    FetchedVideoInfo (Err er)   -> Debug.log "fetching video info failed" er |> \_ -> (m, Effects.none)
    MouseMoved mm               -> ( { m | mouseMoved = mm }, Effects.none )
    OverControls o      -> ( { m | overControls = o }, Effects.none)
    GoFullscreen        -> (m, goFullscreen m |> Effects.task)
    SeekDone            -> ( { m | doSeek = False }, Effects.none )
    SeekTo t            ->
        let
            m' = { m | playStartTime = t, playState = Seeking, doSeek = True }
        in
            (m' , setPlay m' True |> Effects.task )

setPlay : Model -> Bool -> Task Effects.Never Action
setPlay m play = Native.VideoPlayer.setPlay m play

goFullscreen : Model -> Task Effects.Never Action
goFullscreen m = Native.VideoPlayer.goFullscreen m

attachHls : Model -> Task Effects.Never Action
attachHls m = Native.VideoPlayer.attachHls m

wantControls : Model -> Bool
wantControls m = m.mouseMoved || m.overControls

view : Address Action -> Model -> Html
view aa m =
    let
        targetCurrentTime = JD.at ["target", "currentTime"] JD.float
        cursorStyle = ( "cursor", if wantControls m then "auto" else "none" )
    in
        Html.div [ HA.class "embed-responsive embed-responsive-16by9 video-responsive", HA.style [ cursorStyle ] ]
            [ Html.div [ HA.id <| m.playerId ++ "-container" ]
                [ Html.video
                    [ HA.id m.playerId
                    , HA.poster <| Server.videoFrameUrl m.fileId 200
                    , HA.controls False
                    , HE.on "playing" (JD.succeed ()) (\() -> Signal.message aa (PlayStateChanged Playing))
                    , HE.on "pause" (JD.succeed ()) (\() -> Signal.message aa (PlayStateChanged Paused))
                    , HE.on "timeupdate" targetCurrentTime (\t -> Signal.message aa (PlayTimeChanged t))
                    ]
                    [ Html.text "Kein Video hier?" ]
                , controls aa m
                ]
            ]

controls : Address Action -> Model -> Html
controls aa m =
    let
        playClick = onClick' aa (if m.playState == Paused then Play else Pause)
        opacity = if wantControls m then 1 else 0
        progress duration =
            let
                clickLocation : JD.Decoder (Int, Int) -- x and width
                clickLocation =
                    JD.object2 (,)
                        (JD.at ["offsetX"] JD.int)
                        (JD.at ["currentTarget", "clientWidth"] JD.int)
                pct = currentTime m / duration * 100
                seek (x, w) = Signal.message aa (SeekTo (toFloat x / toFloat w * duration))
            in
                Html.div [ HA.class "video-progress", HE.on "click" clickLocation seek ]
                    [ Html.div
                        [ HA.style [("width", toString pct ++ "%"), ("height", "100%"), ("background-color", "red" )] ]
                        []
                    ]

        showTime duration =
            let
                go unit t = if t > unit
                    then
                        let
                            x = floor <| t / unit
                            ts = toString x
                        in
                            (t - unit * toFloat x, if String.length ts < 2 then "0" ++ ts else ts)
                    else
                        (t, "00")

                fmt t =
                    let
                        (t' , hs) = go Time.hour (t * Time.second)
                        (t'', ms) = go Time.minute t'
                        (_  , ss) = go Time.second t''
                    in
                        (hs ++ ":" ++ ms ++ ":" ++ ss)

            in
                Html.span [ HA.class "video-playtime" ]
                    [ Html.text <| fmt (currentTime m) ++ " / " ++ fmt duration ]

    in
        Html.div [ HA.class "embed-responsive-item" ]
            [ Html.div
                [ HA.class "video-controls"
                , HA.style [ ("opacity", toString opacity) ]
                , HE.onMouseOver aa (OverControls True)
                , HE.onMouseLeave aa (OverControls False)
                ]
                [ Html.div
                    [ HA.class "clearfix"
                    , HA.style [ ("position", "relative"), ("width", "95%"), ("margin", "0 auto") ]
                    ]
                    <| List.filterMap identity
                        [ Maybe.map (\vi -> progress vi.duration) m.videoInfo
                        , Just <| Html.button [ playClick, HA.class "video-button" ]
                            [ Html.span
                                [ HA.classList
                                    [ ( "glyphicon", True)
                                    , ( "glyphicon-play", m.playState == Paused )
                                    , ( "glyphicon-pause", m.playState == Playing )
                                    ]
                                ] []
                            ]
                        , Maybe.map (\vi -> showTime vi.duration) m.videoInfo
                        , Just <| Html.button [ onClick' aa GoFullscreen, HA.class "video-button pull-right" ]
                            [ Html.span [ HA.class "glyphicon glyphicon-fullscreen" ] []
                            ]
                        ]
                ]
            ]
