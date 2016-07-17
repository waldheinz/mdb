
module Album exposing (
    ListModel, initialListModel, withListFilter, ListAction, updateList, viewList, listPagination
    )

import Html exposing ( Html )
import Html.Attributes as HA

import File
import Listing
import Route exposing ( clickRoute )
import Server
import Types exposing ( .. )

type alias ListModel =
    { albums        : Listing.Model Album
    , albumFilter   : WhichAlbums
    , order         : String
    , direction     : String
    }

initialListModel : ListModel
initialListModel =
    { albums        = Listing.mkModel (Server.fetchAlbums AllAlbums "created" "DESC")
    , albumFilter   = AllAlbums
    , order         = "created"
    , direction     = "DESC"
    }

withListFilter : WhichAlbums -> ListModel -> (ListModel, Effects ListAction)
withListFilter flt m =
    if (flt == m.albumFilter)
        then (m, Listing.refresh m.albums |> Effects.map AlbumListing)
        else
            let
                (as', afx)  = Listing.withFetchTask (Server.fetchAlbums flt m.order m.direction) m.albums
            in
                ( { m | albumFilter = flt, albums = as' }, Effects.map AlbumListing afx )

type ListAction
    = AlbumListing (Listing.Action Album)

viewList : Address ListAction -> ListModel -> Html
viewList aa m =
    let
        oneAlbum a =
            Html.div [ HA.class "col-xs-2" ]
                [ Html.a (HA.class "thumbnail" :: clickRoute (Route.Album a.albumId Nothing))
                    [ Maybe.withDefault 0 a.poster |> File.thumb File.Square
                    , Html.span [ HA.class "item-name" ] [ Html.text a.name ]
                    ]
                ]
    in
        List.map oneAlbum m.albums.items |> Html.div [ HA.class "row" ]

listPagination : Address ListAction -> ListModel -> Html
listPagination aa m = Listing.pagination (Signal.forwardTo aa AlbumListing) m.albums

updateList : ListAction -> ListModel -> (ListModel, Effects ListAction)
updateList a m = case a of
    AlbumListing la -> Listing.update la m.albums |> \(as', fx) -> ( { m | albums = as' }, Effects.map AlbumListing fx )
