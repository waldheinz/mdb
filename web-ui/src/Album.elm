
module Album (
    albumDecoder, albumListDecoder,
    ListAction(AlbumSelected), viewList
    ) where

import Dict exposing ( Dict )
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD exposing ( (:=) )
import Signal exposing ( Address )

import Types exposing ( AlbumId, Album )
import Utils exposing ( onClick' )

albumDecoder : JD.Decoder Album
albumDecoder = JD.object1 Album
    ( "albumName" := JD.string )

albumListDecoder : JD.Decoder (AlbumId, Album)
albumListDecoder = JD.object2 (,) ("albumId" := JD.int) albumDecoder

type ListAction
    = AlbumSelected AlbumId

viewList : Address ListAction -> Dict AlbumId Album -> Html
viewList aa d =
    let
        oneAlbum (aid, a) =
            Html.div [ HA.class "col-xs-2" ]
                [ Html.a [ HA.class "thumbnail", onClick' aa (AlbumSelected aid), HA.href "#" ]
                    [ Html.text a.name ]
                ]
    in
        Dict.toList d |> List.map oneAlbum |> Html.div [ HA.class "row" ]
