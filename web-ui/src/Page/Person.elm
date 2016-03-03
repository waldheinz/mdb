
module Page.Person (
    Model, Action, view, update, onMount, initialModel
    ) where

import Dict exposing ( Dict )
import Effects exposing ( Effects )
import Html exposing ( Html )
import Http
import Signal
import Task

import Album exposing (ListAction(..))
import Server exposing ( ApiList, WhichAlbums(..) )
import Types exposing ( Album, AlbumId, PersonId )

type alias Model =
    { personId  : PersonId
    , albums    : Dict AlbumId Album
    }

initialModel : Model
initialModel =
    { personId  = 0
    , albums    = Dict.empty
    }

type Action
    = NoOp
    | AlbumsLoaded (Result Http.Error (ApiList (AlbumId, Album)))
    | AlbumListAction Album.ListAction

onMount : PersonId -> Effects Action
onMount pid = Server.fetchAlbums (PersonAlbums pid) |> Task.toResult |> Task.map AlbumsLoaded |> Effects.task

view : Signal.Address Action -> Model -> Html
view aa m =
    Html.div []
        [ Html.text "Person hier"
        , Album.viewList (Signal.forwardTo aa AlbumListAction) m.albums
        ]

update : Action -> Model -> Model
update a m = case a of
    NoOp                                -> m
    AlbumsLoaded (Err err)              -> Debug.log "loading albums failed" err |> \_ -> m
    AlbumsLoaded (Ok al)                -> { m | albums = Dict.fromList al.items }
    AlbumListAction (AlbumSelected aid) -> m
