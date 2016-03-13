
module Main ( main ) where

import Effects exposing ( Effects, Never )
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Task
import Signal
import StartApp
import TransitRouter exposing ( WithRoute, getTransition )
import TransitStyle

import Page.Album
import Page.Home
import Page.Person
import Page.Video
import Route exposing ( Route(..) )
import Server
import Utils exposing ( onClick' )
import VideoPlayer as VP

port initialPath : String

type alias LoginModel =
    { userName  : String
    , password  : String
    }

initialLoginModel : LoginModel
initialLoginModel = { userName = "", password = "" }

type alias Model = WithRoute Route
    { homePageModel     : Page.Home.Model
    , personPageModel   : Page.Person.Model
    , videoPageModel    : Page.Video.Model
    , userName          : Maybe String
    , loginModel        : LoginModel
    }

initialModel : Model
initialModel =
  { transitRouter   = TransitRouter.empty Home
  , homePageModel   = Page.Home.initialModel
  , personPageModel = Page.Person.initialModel
  , videoPageModel  = Page.Video.initialModel
  , userName        = Nothing
  , loginModel      = initialLoginModel
  }

type Action
    = RouterAction (TransitRouter.Action Route)
    | PageHomeAction Page.Home.Action
    | PagePersonAction Page.Person.Action
    | PageVideoAction Page.Video.Action
    | GotUser (Result Http.Error String)
    | SetLoginUser String
    | SetLoginPass String
    | DoLogin

actions : Signal Action
actions = Signal.mergeMany
    [ Signal.map RouterAction TransitRouter.actions
    , Signal.map (Page.Video.PlayerAction >> PageVideoAction) VP.input
    ]

mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route m = case route of
    Route.Home                  ->
        let
            (hp', hpfx) = Page.Home.onMount m.homePageModel
        in
            ( { m | homePageModel = hp' }, Effects.map PageHomeAction hpfx )

    Route.Person pid            ->
        let
            (pp', ppfx) = Page.Person.onMount pid m.personPageModel
        in
            ( { m | personPageModel = pp' }, ppfx |> Effects.map PagePersonAction )

    Route.PersonAlbum pid aid   ->
        let
            ppm         = m.personPageModel
            (ap', apfx) = Page.Album.onMount aid ppm.albumPage
            ppm'        = { ppm | albumPage = ap' }
        in
            ( { m | personPageModel = ppm' }, Effects.map (Page.Person.AlbumAction >> PagePersonAction) apfx)

    Route.Video fid ->
        let
            (vp', pvfx) = Page.Video.onMount fid m.videoPageModel
        in
            ( { m | videoPageModel = vp' }, Effects.map PageVideoAction pvfx)

routerConfig : TransitRouter.Config Route Action Model
routerConfig =
    { mountRoute    = mountRoute
    , getDurations  = \_ _ _ -> (50, 200)
    , actionWrapper = RouterAction
    , routeDecoder  = Route.decode
    }

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    RouterAction ra         -> TransitRouter.update routerConfig ra m
    PageHomeAction ha       -> ( { m | homePageModel = Page.Home.update ha m.homePageModel }, Effects.none)
    PagePersonAction pa     ->
        let
            (pp', ppfx) = Page.Person.update pa m.personPageModel
        in
            ({ m | personPageModel = pp' }, Effects.map PagePersonAction ppfx )

    PageVideoAction va      ->
        let
            (vp', vpfx) = Page.Video.update va m.videoPageModel
        in
            ( { m | videoPageModel = vp' }, Effects.map PageVideoAction vpfx )

    GotUser (Err x)         -> Debug.log "error getting user" x |> \_ -> (m, Effects.none)
    GotUser (Ok name)       -> ( { m | userName = Just name }, Effects.none )
    SetLoginUser u          ->
        let
            lm  = m.loginModel
            lm' = { lm | userName = u }
        in
            ({ m | loginModel = lm' }, Effects.none)

    SetLoginPass p          ->
        let
            lm  = m.loginModel
            lm' = { lm | password = p }
        in
            ({ m | loginModel = lm' }, Effects.none)

    DoLogin ->
        (m, Server.doLogin m.loginModel |> Task.toResult |> Task.map GotUser |> Effects.task )

loginPage : Signal.Address Action -> LoginModel -> Html
loginPage aa m =
    Html.div [ HA.class "jumbotron" ]
        [ Html.h1 [] [ Html.text "Login Required" ]
        , Html.form [ HA.class "clearfix" ]
            [ Html.div [ HA.class "form-group" ]
                [ Html.label [] [ Html.text "Username" ]
                , Html.input
                    [ HA.class "form-control", HA.id "mdbUser"
                    , HE.on "change" HE.targetValue (\p -> Signal.message aa (SetLoginUser p)) ] []
                ]
            , Html.div [ HA.class "form-group" ]
                [ Html.label [] [ Html.text "Password" ]
                , Html.input
                    [ HA.type' "password", HA.class "form-control", HA.id "mdbPassword"
                    , HE.on "change" HE.targetValue (\p -> Signal.message aa (SetLoginPass p)) ] []
                ]
            , Html.button [ HA.type' "button", HA.class "btn btn-primary pull-right", onClick' aa DoLogin ]
                [ Html.text "login" ]
            ]
        ]

view : Signal.Address Action -> Model -> Html
view aa m =
    let
        routedContent = case TransitRouter.getRoute m of
            Home                -> Page.Home.view (Signal.forwardTo aa PageHomeAction) m.homePageModel
            Person pid          -> Page.Person.view (Signal.forwardTo aa PagePersonAction) m.personPageModel
            PersonAlbum pid aid -> Page.Album.view (Signal.forwardTo aa (Page.Person.AlbumAction >> PagePersonAction) ) m.personPageModel.albumPage
            Video _             -> Page.Video.view (Signal.forwardTo aa PageVideoAction) m.videoPageModel

        mainContent = case m.userName of
            Nothing -> loginPage aa m.loginModel
            Just _  ->
                Html.div [ HA.style <| TransitStyle.fadeSlideLeft 100 <| getTransition m ] [ routedContent ]

    in
        Html.div [ HA.class "container" ]
            [ mainContent ]

init : (Model, Effects Action)
init =
    let
        (m, trfx)   = TransitRouter.init routerConfig initialPath initialModel
        checkLogin  = Server.checkUser |> Task.toResult |> Task.map GotUser |> Effects.task
    in
        (m, Effects.batch [ checkLogin, trfx ] )

app : StartApp.App (WithRoute Route Model)
app = StartApp.start
    { init      = init
    , update    = update
    , view      = view
    , inputs    = [ actions ]
    }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
