
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
import Route exposing ( Route(..) )
import Server exposing ( ApiList, fetchPersons )
import Types exposing ( Person, PersonId, WithPersons )

port initialPath : String

type alias Model = WithPersons (WithRoute Route
    { personPageModel : Page.Person.Model
    })

initialModel : Model
initialModel =
  { transitRouter   = TransitRouter.empty Home
  , persons         = Dict.empty
  , personPageModel = Page.Person.initialModel
  }

type Action
    = RouterAction (TransitRouter.Action Route)
    | UpdatePersons (Result Http.Error (ApiList (PersonId, Person)))
    | PageHomeAction Page.Home.Action
    | PagePersonAction Page.Person.Action

actions : Signal Action
actions =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.map RouterAction TransitRouter.actions

mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route m = case route of
    Route.Home                  -> (m, Server.fetchPersons |> Task.toResult |> Task.map UpdatePersons |> Effects.task)
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
    UpdatePersons (Ok pl)   -> ({m | persons = Dict.fromList pl.items}, Effects.none)
    UpdatePersons (Err e)   -> Debug.log "fetching persons failed" e |> \_ -> ( m, Effects.none )
    PageHomeAction ha       -> (Page.Home.update ha m, Effects.none)
    PagePersonAction pa     ->
        let
            (pp', ppfx) = Page.Person.update pa m.personPageModel
        in
            ({ m | personPageModel = pp' }, Effects.map PagePersonAction ppfx )

view : Signal.Address Action -> Model -> Html
view aa m =
    let
        mainContent = case TransitRouter.getRoute m of
            Home                -> Page.Home.view (Signal.forwardTo aa PageHomeAction) m
            Person pid          -> Page.Person.view (Signal.forwardTo aa PagePersonAction) m.personPageModel
            PersonAlbum pid aid -> Page.Album.view (Signal.forwardTo aa (Page.Person.AlbumAction >> PagePersonAction) ) m.personPageModel.albumPage

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
