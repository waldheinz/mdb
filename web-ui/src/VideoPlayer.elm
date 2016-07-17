
module VideoPlayer exposing (
    Model, initialModel, setVideo, view,
    Action, update
    )

import Json.Decode as JD
import Json.Encode as JE
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Mouse
import String
import Task exposing ( Task )
import Time

import Server
import Types exposing (..)
import Utils exposing ( onClick' )
import Native.VideoPlayer

type PlayState = Playing | Paused | Seeking

type alias Model =
    { videoBaseUrl      : String
    , fileId            : FileId
    , playTime          : Float
    , playStartTime     : Float
    , playState         : PlayState
    , playerId          : String
    , videoInfo         : Maybe Container
    , mouseMoved        : Bool
    , overControls      : Bool              -- ^ does the mouse cursor hover over the control panel?
    , doSeek            : Bool              -- ^ if there is a seek operation pending (so we have to set src again)
    , lastRecPlayPos    : Float
    }

initialModel : String -> Model
initialModel playerId =
    { videoBaseUrl      = ""
    , fileId            = 0
    , playTime          = 0
    , playStartTime     = 0
    , playState         = Paused
    , playerId          = playerId
    , videoInfo         = Nothing
    , mouseMoved        = True
    , overControls      = False
    , doSeek            = True
    , lastRecPlayPos    = 0
    }

-- input : Signal Action
-- input = Time.since (3 * Time.second) (Mouse.position) |> Signal.map MouseMoved

setVideo : FileId -> Model -> (Model, Cmd Action)
setVideo fid m =
    let
        m' = if fid == m.fileId
                then m
                else
                    { m
                    | fileId            = fid
                    , playTime          = 0
                    , playStartTime     = 0
                    , videoBaseUrl      = Server.videoBaseUrl fid ++ "/dash/index.mpd"
                    , videoInfo         = Nothing
                    , doSeek            = True
                    , lastRecPlayPos    = 0
                    }
        -- fx = Server.fetchContainerForFile fid |> Task.toResult |> Task.map FetchedVideoInfo |> Effects.task
        fx = attachDash m'
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
    | PlayPosRecorded (Result Http.Error ())

currentTime : Model -> Float
currentTime m = case m.playState of
    Paused  -> m.playStartTime + m.playTime
    Playing -> m.playStartTime + m.playTime
    Seeking -> m.playStartTime

recordPlayPos : Model -> (Model, Cmd Action)
recordPlayPos m =
    let
        t   = currentTime m
        pp  = JE.object [ ("fileId", encodeFileId m.fileId), ("playPos", JE.float t ) ]
        m'  = { m | lastRecPlayPos = t }
    in
        if t > (m.lastRecPlayPos + 10)
            then (m', Server.recordVideoPlay pp |> Task.perform (Err >> PlayPosRecorded) (Ok >> PlayPosRecorded))
            else (m, Cmd.none)

update : Action -> Model -> (Model, Cmd Action)
update a m = case a of
    NoOp                -> ( m, Cmd.none )
    Play                -> ( { m | doSeek = False }, setPlay m True )
    Pause               -> ( m , setPlay m False )
    PlayStateChanged s  -> ( { m | playState = s }, Cmd.none )
    PlayTimeChanged t   -> { m | playTime = t } |> recordPlayPos
    FetchedVideoInfo (Ok v) -> ( { m | videoInfo = Just v }, Cmd.none )
    FetchedVideoInfo (Err er)   -> Debug.log "fetching video info failed" er |> \_ -> (m, Cmd.none)
    MouseMoved mm               -> ( { m | mouseMoved = mm }, Cmd.none )
    OverControls o      -> ( { m | overControls = o }, Cmd.none)
    GoFullscreen        -> (m, goFullscreen m )
    SeekDone            -> ( { m | doSeek = False }, Cmd.none )
    SeekTo t            ->
        let
            m' = { m | playStartTime = t, playState = Seeking, doSeek = True, lastRecPlayPos = t }
        in
            (m' , setPlay m' True )
    PlayPosRecorded (Err ex)    -> Debug.log "recording play pos failed" ex |> \_ -> ( m , Cmd.none)
    PlayPosRecorded (Ok ())     -> ( m , Cmd.none)

setPlay : Model -> Bool -> Cmd Action
setPlay m play = Native.VideoPlayer.setPlay m play

goFullscreen : Model -> Cmd Action
goFullscreen m = Native.VideoPlayer.goFullscreen m

attachHls : Model -> Cmd Action
attachHls m = Native.VideoPlayer.attachHls m

attachDash : Model -> Cmd Action
attachDash m = Native.VideoPlayer.attachDash m

wantControls : Model -> Bool
wantControls m = m.mouseMoved || m.overControls

view : Model -> Html Action
view m =
    let
        targetCurrentTime = JD.at ["target", "currentTime"] JD.float
        cursorStyle = ( "cursor", if wantControls m then "auto" else "none" )
    in
        Html.div [ HA.class "embed-responsive embed-responsive-16by9" ]
            [ Html.div
                [ HA.id <| m.playerId ++ "-container"
                , HA.class "embed-responsive-item video-container"
                , HA.style [ cursorStyle ]
                ]
                [ Html.video
                    [ HA.id m.playerId
                    , HA.poster <| Server.videoFrameUrl m.fileId 200
                    , HA.controls True
                    , HA.autoplay False
                    ]
                    [ Html.text "Kein Video hier?" ]
--                , controls aa m
                ]
            ]

controls : Model -> Html Action
controls m =
    let
        playClick = onClick' (if m.playState == Paused then Play else Pause)
        opacity = if wantControls m then 1 else 0
        progress duration =
            let
                -- clickSeek : JD.Decoder (Int, Int) -- x and width
                clickSeek =
                    let
                        pos = JD.object2 (,)
                            (JD.at ["offsetX"] JD.int)
                            (JD.at ["currentTarget", "clientWidth"] JD.int)
                        seek (x, w) = SeekTo (toFloat x / toFloat w * duration)
                    in
                        JD.map seek pos

                pct = currentTime m / duration * 100

            in
                Html.div [ HA.class "video-progress", HE.on "click" clickSeek ]
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
                , HE.onMouseOver (OverControls True)
                , HE.onMouseLeave (OverControls False)
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
                        , Just <| Html.button [ onClick' GoFullscreen, HA.class "video-button pull-right" ]
                            [ Html.span [ HA.class "glyphicon glyphicon-fullscreen" ] []
                            ]
                        ]
                ]
            ]
