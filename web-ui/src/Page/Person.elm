
module Page.Person (
    Model, Action(AlbumAction), view, update, onMount, initialModel
    ) where

import Dict exposing ( Dict )
import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Signal
import Task

import Album exposing (ListAction(..))
import File
import Page.Album
import Route
import Server exposing ( ApiList, WhichAlbums(..) )
import Types exposing ( .. )

type alias Model =
    { personId      : PersonId
    , albums        : Dict AlbumId Album
    , albumPage     : Page.Album.Model
    , randomFiles   : File.ListModel
    }

initialModel : Model
initialModel =
    { personId      = 0
    , albums        = Dict.empty
    , albumPage     = Page.Album.initialModel
    , randomFiles   = File.mkListModel AllFiles
    }

type Action
    = NoOp
    | AlbumsLoaded (Result Http.Error (ApiList (AlbumId, Album)))
    | AlbumListAction Album.ListAction
    | AlbumAction Page.Album.Action
    | FileListAction File.ListAction

noOp : Effects () -> Effects Action
noOp = Effects.map (\() -> NoOp)

onMount : PersonId -> Model -> (Model, Effects Action)
onMount pid m =
    let
        (rf', rffx) = File.setListFilter (PersonNoAlbum pid) m.randomFiles
        fa          = Server.fetchAlbums (PersonAlbums pid) |> Task.toResult |> Task.map AlbumsLoaded |> Effects.task
    in
        ( { m | personId = pid, randomFiles = rf' }
        , Effects.batch [ fa, Effects.map FileListAction rffx ]
        )

view : Signal.Address Action -> Model -> Html
view aa m =
    Html.div []
        [ Html.h1 [ HA.class "page-lead" ] [ Html.text "Person" ]
        , Html.h2 [] [ Html.text "Albums" ]
        , Album.viewList (Signal.forwardTo aa AlbumListAction) m.albums
        , Html.h2 [] [ Html.text "Random Files" ]
        , File.viewList (Signal.forwardTo aa FileListAction) m.randomFiles
        ]

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    NoOp                                -> (m, Effects.none)
    AlbumsLoaded (Err err)              -> Debug.log "loading albums failed" err |> \_ -> (m, Effects.none)
    AlbumsLoaded (Ok al)                -> ( { m | albums = Dict.fromList al.items }, Effects.none )
    AlbumListAction (AlbumSelected aid) -> ( m, Route.goRoute (Route.PersonAlbum m.personId aid) |> noOp )
    FileListAction (File.VideoSelected fid) ->
        (m, Route.goRoute (Route.Video fid) |> noOp)
    FileListAction fla                  -> ( { m | randomFiles = File.updateListModel fla m.randomFiles }, Effects.none)
    AlbumAction aa                      ->
        let
            (ap', apfx) = Page.Album.update aa m.albumPage
        in
            ( { m | albumPage = ap' }, Effects.map AlbumAction apfx )
