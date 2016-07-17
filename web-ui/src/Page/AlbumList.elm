
module Page.AlbumList exposing (
    Model, initialModel, view,
    Action, update, onMount
    )

import Html exposing ( Html )
import Html.Attributes as HA

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

noOp : Effects () -> Effects Action
noOp = Effects.map (\() -> NoOp)

onMount : Model-> (Model, Effects Action)
onMount m =
    let
        (as', afx)  = Album.withListFilter AllAlbums m.albums
    in
        ( { m | albums = as' }
        , Effects.map AlbumListAction afx
        )

view : Address Action -> Model -> Html
view aa m =
    Html.div [ HA.class "container" ]
        [ Html.h1 [ HA.class "page-lead" ] [ Html.text <| "Albums" ]
        , Html.div [ HA.class "text-center" ]
            [ Album.listPagination (Signal.forwardTo aa AlbumListAction) m.albums ]
        , Album.viewList (Signal.forwardTo aa AlbumListAction) m.albums
        ]

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    NoOp                -> (m, Effects.none)
    AlbumListAction la  ->
        let
            (al', fx)   = Album.updateList la m.albums
        in
            ({ m | albums = al' }, Effects.map AlbumListAction fx)
