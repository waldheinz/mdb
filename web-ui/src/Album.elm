
module Album (
    ListModel, initialListModel, withListFilter, ListAction, updateList, viewList
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Signal exposing ( Address )

import File
import Listing
import Route exposing ( clickRoute )
import Server
import Types exposing ( .. )

type alias ListModel =
    { albums        : Listing.Model Album
    , albumFilter   : WhichAlbums
    }

initialListModel : ListModel
initialListModel = { albums = Listing.mkModel (Server.fetchAlbums AllAlbums), albumFilter = AllAlbums }

withListFilter : WhichAlbums -> ListModel -> (ListModel, Effects ListAction)
withListFilter flt m =
    let
        (as', afx)  = Listing.withFetchTask (Server.fetchAlbums flt) m.albums
    in
        ( { m | albumFilter = flt, albums = as' }, Effects.map AlbumListing afx )

type ListAction
    = AlbumListing (Listing.Action Album)

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
        List.map oneAlbum m.albums.items |> Html.div [ HA.class "row" ]

updateList : ListAction -> ListModel -> (ListModel, Effects ListAction)
updateList a m = case a of
    AlbumListing la -> Listing.update la m.albums |> \(as', fx) -> ( { m | albums = as' }, Effects.map AlbumListing fx )
