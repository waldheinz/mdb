
module Main exposing ( main )

import Navigation
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Task

import Navbar
import Page.Album
import Page.AlbumList
import Page.Home
import Page.Person
import Page.Series exposing ( WithSeries )
import Page.SeriesEpisodes exposing ( WithEpisodes )
import Page.SeriesSeasons exposing ( WithSeasons )
import Page.Video
import Route exposing ( Route(..) )
import Server
import Utils exposing ( onClick_ )
import VideoPlayer as VP

-- port initialPath : String

type alias LoginModel =
    { userName  : String
    , password  : String
    }

initialLoginModel : LoginModel
initialLoginModel = { userName = "", password = "" }

type alias Model = WithEpisodes (WithSeasons (WithSeries
    { currentRoute      : Route
    , homePageModel     : Page.Home.Model
    , personPageModel   : Page.Person.Model
    , videoPageModel    : Page.Video.Model
    , userName          : Maybe String
    , loginModel        : LoginModel
    , albumPageModel    : Page.Album.Model
    , albumListPageModel    : Page.AlbumList.Model
    }))

initialModel : Model
initialModel =
  { currentRoute        = Home
  , homePageModel       = Page.Home.initialModel
  , personPageModel     = Page.Person.initialModel
  , videoPageModel      = Page.Video.initialModel
  , userName            = Nothing
  , loginModel          = initialLoginModel
  , pageSeriesModel     = Page.Series.initialModel
  , pageSeasonsModel    = Page.SeriesSeasons.initialModel
  , pageEpisodesModel   = Page.SeriesEpisodes.initialModel
  , albumPageModel      = Page.Album.initialModel
  , albumListPageModel  = Page.AlbumList.initialModel
  }

type Action
    = PageHomeAction Page.Home.Action
    | PagePersonAction Page.Person.Action
    | PageSeriesAction Page.Series.Action
    | PageSeasonsAction Page.SeriesSeasons.Action
    | PageEpisodesAction Page.SeriesEpisodes.Action
    | PageVideoAction Page.Video.Action
    | PageAlbumAction Page.Album.Action
    | PageAlbumListAction Page.AlbumList.Action
    | GotUser (Result Http.Error String)
    | SetLoginUser String
    | SetLoginPass String
    | DoLogin
    | NavbarAction Navbar.Action
    | NoOp

mountRoute : Route -> Model -> (Model, Cmd Action)
mountRoute route m =
    case route of
        Route.Home                  ->
            let
                (hp', hpfx) = Page.Home.onMount m.homePageModel
            in
                ( { m | homePageModel = hp' }, Cmd.map PageHomeAction hpfx )

        Route.AlbumList ->
            let
                (al', fx) = Page.AlbumList.onMount m.albumListPageModel
            in
                ( { m | albumListPageModel = al' } , Cmd.map PageAlbumListAction fx)

        Route.Person pid            ->
            let
                (pp', ppfx) = Page.Person.onMount pid m.personPageModel
            in
                ( { m | personPageModel = pp' }, ppfx |> Cmd.map PagePersonAction )

        Route.Album aid mfid    ->
            let
                (ap', apfx) = Page.Album.onMount aid mfid m.albumPageModel
            in
                ( { m | albumPageModel = ap' }, Cmd.map PageAlbumAction apfx)

        Route.Series ->
            let
                (m', psfx)  = Page.Series.onMount m
            in
                (m', Cmd.map PageSeriesAction psfx)

        Route.SeriesSeasons sid ->
            Page.SeriesSeasons.onMount sid m |> \(m', fx) -> (m', Cmd.map PageSeasonsAction fx)

        Route.SeriesEpisodes r a ->
            Page.SeriesEpisodes.onMount r a m |> \(m', fx) -> (m', Cmd.map PageEpisodesAction fx)

        Route.Video fid ->
            let
                (vp', pvfx) = Page.Video.onMount fid m.videoPageModel
            in
                ( { m | videoPageModel = vp' }, Cmd.map PageVideoAction pvfx)

update : Action -> Model -> (Model, Cmd Action)
update a m = case a of
    NoOp                    -> ( m, Cmd.none )
    PageHomeAction ha       -> ( { m | homePageModel = Page.Home.update ha m.homePageModel }, Cmd.none)
    PageAlbumAction a       -> Page.Album.update a m.albumPageModel
                                |> \(ap', fx) -> ( { m | albumPageModel = ap'}, Cmd.map PageAlbumAction fx)
    PageAlbumListAction a -> Page.AlbumList.update a m.albumListPageModel
                                |> \(ap', fx) -> ( { m | albumListPageModel = ap'}, Cmd.map PageAlbumListAction fx)

    PagePersonAction pa     ->
        let
            (pp', ppfx) = Page.Person.update pa m.personPageModel
        in
            ({ m | personPageModel = pp' }, Cmd.map PagePersonAction ppfx )

    PageSeriesAction sa     -> Page.Series.update sa m |> \(m', psfx) -> (m', Cmd.map PageSeriesAction psfx)
    PageSeasonsAction sa ->
        Page.SeriesSeasons.update sa m |> \(m', psfx) -> (m', Cmd.map PageSeasonsAction psfx)

    PageEpisodesAction ea ->
        Page.SeriesEpisodes.update ea m |> \(m', psfx) -> (m', Cmd.map PageEpisodesAction psfx)

    PageVideoAction va      ->
        let
            (vp', vpfx) = Page.Video.update va m.videoPageModel
        in
            ( { m | videoPageModel = vp' }, Cmd.map PageVideoAction vpfx )

    GotUser (Err x)         -> Debug.log "error getting user" x |> \_ -> (m, Cmd.none)
    GotUser (Ok name)       -> ( { m | userName = Just name }, Cmd.none )
    SetLoginUser u          ->
        let
            lm  = m.loginModel
            lm' = { lm | userName = u }
        in
            ({ m | loginModel = lm' }, Cmd.none)

    SetLoginPass p          ->
        let
            lm  = m.loginModel
            lm' = { lm | password = p }
        in
            ({ m | loginModel = lm' }, Cmd.none)

    DoLogin ->
        (m, Server.doLogin m.loginModel |> Task.perform (Err >> GotUser) (Ok >> GotUser) )

    NavbarAction (Navbar.LogOut) ->
        ({ m | userName = Nothing }, Server.doLogout |> Task.perform (\_ -> NoOp) (\_ -> NoOp) )
    NavbarAction (Navbar.RouteMsg msg) -> (m, Route.handleMsg msg)

loginPage : LoginModel -> Html Action
loginPage m =
    Html.div [ HA.class "jumbotron" ]
        [ Html.h1 [] [ Html.text "Login Required" ]
        , Html.form [ HA.class "clearfix" ]
            [ Html.div [ HA.class "form-group" ]
                [ Html.label [] [ Html.text "Username" ]
                , Html.input
                    [ HA.class "form-control", HA.id "mdbUser" , HE.onInput SetLoginUser ] []
                ]
            , Html.div [ HA.class "form-group" ]
                [ Html.label [] [ Html.text "Password" ]
                , Html.input
                    [ HA.type' "password", HA.class "form-control", HA.id "mdbPassword" , HE.onInput SetLoginPass ] []
                ]
            , Html.button [ HA.type' "button", HA.class "btn btn-primary pull-right", onClick_ DoLogin ]
                [ Html.text "login" ]
            ]
        ]

view : Model -> Html Action
view m =
    let
        routedContent = case m.currentRoute of
            Home                -> Html.App.map PageHomeAction (Page.Home.view m.homePageModel)
            Person _            -> Html.App.map PagePersonAction (Page.Person.view m.personPageModel)
            Album _ _           -> Html.App.map PageAlbumAction (Page.Album.view m.albumPageModel)
            AlbumList           -> Html.App.map PageAlbumListAction (Page.AlbumList.view m.albumListPageModel)
            Series              -> Html.App.map PageSeriesAction (Page.Series.view m)
            SeriesSeasons _     -> Html.App.map PageSeasonsAction (Page.SeriesSeasons.view m)
            SeriesEpisodes _ _  -> Html.App.map PageEpisodesAction (Page.SeriesEpisodes.view m)
            Video _             -> Html.App.map PageVideoAction (Page.Video.view m.videoPageModel)

    in
        case m.userName of
            Nothing -> Html.div [ HA.class "container" ] [ loginPage m.loginModel ]
            Just _  -> Html.div [ ]
                [ Html.App.map NavbarAction (Navbar.view m.userName m.currentRoute)
                , routedContent
                ]

init : Route-> (Model, Cmd Action)
init route =
    let
        checkLogin  = Server.checkUser |> Task.perform (Err >> GotUser) (Ok >> GotUser)
        (m, rfx)    = mountRoute route initialModel

    in
        (m, Cmd.batch [ checkLogin, rfx ] )

main : Program Never
main = Navigation.program Route.routeParser
    { init          = init
    , update        = update
    , urlUpdate     = \r m -> mountRoute r { m | currentRoute = r }
    , view          = view
    , subscriptions = \_ -> Sub.none
    }
