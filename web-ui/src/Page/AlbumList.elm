
module Page.AlbumList exposing (
    Model, WithModel, initialModel, view,
    Action, update, onMount
    )

import Html exposing ( Html )
import Html.Attributes as HA

import Album
import Types exposing (..)

type alias Model =
    { albums    : Album.ListModel
    }

type alias WithModel a = { a | albumListModel : Model }

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
        (as_, afx)  = Album.withListFilter AllAlbums m.albums
    in
        ( { m | albums = as_ }
        , Cmd.map AlbumListAction afx
        )

view : WithModel a -> Html Action
view wm =
    let
        m = wm.albumListModel
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-lead" ] [ Html.text <| "Albums" ]
            , Html.div [ HA.class "text-center" ]
                [ Html.map AlbumListAction (Album.listPagination m.albums) ]
            , Html.map AlbumListAction (Album.viewList m.albums)
            ]

update : Action -> WithModel a -> (Action -> b) -> (WithModel a, Cmd b)
update a m wrap = case a of
    NoOp                -> (m, Cmd.none)
    AlbumListAction la  ->
        let
            alm         = m.albumListModel
            (al_, fx)   = Album.updateList la alm.albums
            alm_        = { alm | albums = al_ }
        in
            ({ m | albumListModel = alm_ }, Cmd.map (AlbumListAction >> wrap) fx)
