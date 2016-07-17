
module Page.AlbumList exposing (
    Model, initialModel, view,
    Action, update, onMount
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Html.App

import Album
import Types exposing (..)

type alias Model =
    { albums    : Album.ListModel
    }

initialModel : Model
initialModel =
    { albums   = Album.initialListModel
    }

type Action
    = NoOp
    | AlbumListAction Album.ListAction

noOp : Cmd () -> Cmd Action
noOp = Cmd.map (\() -> NoOp)

onMount : Model-> (Model, Cmd Action)
onMount m =
    let
        (as', afx)  = Album.withListFilter AllAlbums m.albums
    in
        ( { m | albums = as' }
        , Cmd.map AlbumListAction afx
        )

view : Model -> Html Action
view m =
    Html.div [ HA.class "container" ]
        [ Html.h1 [ HA.class "page-lead" ] [ Html.text <| "Albums" ]
        , Html.div [ HA.class "text-center" ]
            [ Html.App.map AlbumListAction (Album.listPagination m.albums) ]
        , Html.App.map AlbumListAction (Album.viewList m.albums)
        ]

update : Action -> Model -> (Model, Cmd Action)
update a m = case a of
    NoOp                -> (m, Cmd.none)
    AlbumListAction la  ->
        let
            (al', fx)   = Album.updateList la m.albums
        in
            ({ m | albums = al' }, Cmd.map AlbumListAction fx)
