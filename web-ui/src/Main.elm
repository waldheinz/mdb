
module Main ( main ) where

import Dict exposing ( Dict )
import Effects exposing ( Effects, Never )
import Html exposing ( Html )
import Http
import Task
import Signal
import StartApp
import TransitRouter exposing ( WithRoute )

import Person exposing ( Person, PersonId )
import Route exposing ( Route(..) )
import Server exposing ( ApiList, fetchPersons )

port initialPath : String

type alias Model = WithRoute Route
    { persons   : Dict PersonId Person
    }

initialModel : Model
initialModel =
  { transitRouter   = TransitRouter.empty Home
  , persons         = Dict.empty
  }

type Action
    = RouterAction (TransitRouter.Action Route)
    | UpdatePersons (Result Http.Error (ApiList (PersonId, Person)))

actions : Signal Action
actions =
  -- use mergeMany if you have other mailboxes or signals to feed into StartApp
  Signal.map RouterAction TransitRouter.actions

init : String -> (Model, Effects Action)
init path = TransitRouter.init routerConfig path initialModel

mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route m = case route of
    Route.Home -> (m, Server.fetchPersons |> Task.toResult |> Task.map UpdatePersons |> Effects.task)

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

view : Signal.Address Action -> Model -> Html
view aa m = Html.div [] [ Html.text "mainView" ]

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
