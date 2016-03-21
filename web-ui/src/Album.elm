
module Album (
    ListAction(AlbumSelected), viewList
    ) where

import Dict exposing ( Dict )
import Html exposing ( Html )
import Html.Attributes as HA
import Signal exposing ( Address )

import File
import Types exposing ( AlbumId, Album )
import Utils exposing ( onClick' )

type ListAction
    = AlbumSelected AlbumId

viewList : Address ListAction -> Dict AlbumId Album -> Html
viewList aa d =
    let
        oneAlbum (aid, a) =
            Html.div [ HA.class "col-xs-2" ]
                [ Html.a [ HA.class "thumbnail", onClick' aa (AlbumSelected aid), HA.href "#" ]
                    [ Maybe.withDefault 0 a.albumPoster |> File.thumb File.Square
                    , Html.span [ HA.class "item-name" ] [ Html.text a.name ]
                    ]
                ]
    in
        Dict.toList d |> List.map oneAlbum |> Html.div [ HA.class "row" ]
