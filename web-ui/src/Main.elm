
module Main ( main ) where

import Dict exposing ( Dict )
import Effects exposing ( Effects, Never )
import Html exposing ( Html )
import Html.Attributes as HA
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
import Server exposing ( ApiList, fetchPersons )
import Types exposing ( .. )

port initialPath : String

type alias Model = WithRoute Route
    { homePageModel     : Page.Home.Model
    , personPageModel   : Page.Person.Model
    , videoPageModel    : Page.Video.Model
    }

initialModel : Model
initialModel =
  { transitRouter   = TransitRouter.empty Home
  , homePageModel   = Page.Home.initialModel
  , personPageModel = Page.Person.initialModel
  , videoPageModel  = Page.Video.initialModel
  }

type Action
    = RouterAction (TransitRouter.Action Route)
    | PageHomeAction Page.Home.Action
    | PagePersonAction Page.Person.Action
    | PageVideoAction Page.Video.Action

actions : Signal Action
actions =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.map RouterAction TransitRouter.actions

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
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Route.decode
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

view : Signal.Address Action -> Model -> Html
view aa m =
    let
        mainContent = case TransitRouter.getRoute m of
            Home                -> Page.Home.view (Signal.forwardTo aa PageHomeAction) m.homePageModel
            Person pid          -> Page.Person.view (Signal.forwardTo aa PagePersonAction) m.personPageModel
            PersonAlbum pid aid -> Page.Album.view (Signal.forwardTo aa (Page.Person.AlbumAction >> PagePersonAction) ) m.personPageModel.albumPage
            Video _             -> Page.Video.view (Signal.forwardTo aa PageVideoAction) m.videoPageModel

    in
        Html.div [ HA.class "container", HA.style <| TransitStyle.fadeSlideLeft 100 <| getTransition m ]
            [ mainContent ]

app = StartApp.start
    { init      = TransitRouter.init routerConfig initialPath initialModel
    , update    = update
    , view      = view
    , inputs    = [ actions ]
    }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
