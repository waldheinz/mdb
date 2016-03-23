
module Album (
    ListModel, initialListModel, withListFilter, ListAction, updateList, viewList
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Signal exposing ( Address )
import Task

import File
import Route exposing ( clickRoute )
import Server
import Types exposing ( .. )
import Utils exposing ( onClick' )

type alias ListModel =
    { albums        : List Album
    , albumFilter   : WhichAlbums
    }

initialListModel : ListModel
initialListModel = { albums = [], albumFilter = AllAlbums }

fetchList : WhichAlbums -> Effects ListAction
fetchList f = Server.fetchAlbums f |> Task.toResult |> Task.map AlbumsLoaded |> Effects.task

withListFilter : WhichAlbums -> ListModel -> (ListModel, Effects ListAction)
withListFilter flt m = ( { m | albumFilter = flt }, fetchList flt )

type ListAction
    = AlbumsLoaded (Result Http.Error (ApiList Album))

viewList : Address ListAction -> ListModel -> Html
viewList aa m =
    let
        oneAlbum a =
            Html.div [ HA.class "col-xs-2" ]
                [ Html.a (HA.class "thumbnail" :: clickRoute (Route.Album a.albumId))
                    [ Maybe.withDefault 0 a.poster |> File.thumb File.Square
                    , Html.span [ HA.class "item-name" ] [ Html.text a.name ]
                    ]
                ]
    in
        List.map oneAlbum m.albums |> Html.div [ HA.class "row" ]

updateList : ListAction -> ListModel -> (ListModel, Effects ListAction)
updateList a m = case a of
    AlbumsLoaded (Err er)   -> Debug.log "fetching albums failed" er |> \_ -> (m, Effects.none)
    AlbumsLoaded (Ok al)    -> ( { m | albums = al.items }, Effects.none )
