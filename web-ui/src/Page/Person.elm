
module Page.Person (
    Model, Action(AlbumAction), view, update, onMount, initialModel
    ) where

import Dict exposing ( Dict )
import Effects exposing ( Effects )
import Html exposing ( Html )
import Http
import Signal
import Task

import Album exposing (ListAction(..))
import Page.Album
import Route
import Server exposing ( ApiList, WhichAlbums(..) )
import Types exposing ( Album, AlbumId, PersonId )

type alias Model =
    { personId  : PersonId
    , albums    : Dict AlbumId Album
    , albumPage : Page.Album.Model
    }

initialModel : Model
initialModel =
    { personId  = 0
    , albums    = Dict.empty
    , albumPage = Page.Album.initialModel
    }

type Action
    = NoOp
    | AlbumsLoaded (Result Http.Error (ApiList (AlbumId, Album)))
    | AlbumListAction Album.ListAction
    | AlbumAction Page.Album.Action

noOp : Effects () -> Effects Action
noOp = Effects.map (\() -> NoOp)

onMount : PersonId -> Model -> (Model, Effects Action)
onMount pid m =
    ( { m | personId = pid }
    , Server.fetchAlbums (PersonAlbums pid) |> Task.toResult |> Task.map AlbumsLoaded |> Effects.task
    )

view : Signal.Address Action -> Model -> Html
view aa m =
    Html.div []
        [ Html.text "Person hier"
        , Album.viewList (Signal.forwardTo aa AlbumListAction) m.albums
        ]

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    NoOp                                -> (m, Effects.none)
    AlbumsLoaded (Err err)              -> Debug.log "loading albums failed" err |> \_ -> (m, Effects.none)
    AlbumsLoaded (Ok al)                -> ( { m | albums = Dict.fromList al.items }, Effects.none )
    AlbumListAction (AlbumSelected aid) -> ( m, Route.goRoute (Route.PersonAlbum m.personId aid) |> noOp )
    AlbumAction aa                      -> ( { m | albumPage = Page.Album.update aa m.albumPage }, Effects.none )
