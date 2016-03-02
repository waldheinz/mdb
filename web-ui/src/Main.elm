
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

import Page.Home
import Page.Person
import Route exposing ( Route(..) )
import Server exposing ( ApiList, fetchPersons )
import Types exposing ( Person, PersonId, WithPersons )

port initialPath : String

type alias Model = WithPersons (WithRoute Route
    {
    })

initialModel : Model
initialModel =
  { transitRouter   = TransitRouter.empty Home
  , persons         = Dict.empty
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

init : String -> (Model, Effects Action)
init path = TransitRouter.init routerConfig path initialModel

mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route m = case route of
    Route.Home          -> (m, Server.fetchPersons |> Task.toResult |> Task.map UpdatePersons |> Effects.task)
    Route.Person pid    -> (m, Server.fetchPerson |> Task.toResult |> Task.map PagePersonAction |> Effects.task)

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
    PagePersonAction pa     -> (m, Effects.none )

view : Signal.Address Action -> Model -> Html
view aa m =
    let
        mainContent = case TransitRouter.getRoute m of
            Home        -> Page.Home.view (Signal.forwardTo aa PageHomeAction) m
            Person pid  -> Page.Person.view (Signal.forwardTo aa PagePersonAction) { personId = pid }
    in
        Html.div [ HA.class "container", HA.style <| TransitStyle.fadeSlideLeft 100 <| getTransition m ]
            [ mainContent ]

app = StartApp.start
    { init = init initialPath
    , update = update
    , view = view
    , inputs = [ actions ]
    }

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
