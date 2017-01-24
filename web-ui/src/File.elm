
module File exposing (
    -- * Files
    AspectRatio(..), thumb,

    -- * File Listings
    ListModel, mkListModel, withImageRouter, setListItemCount,
    ListAction, viewList, listPagination,
    updateListModel, setListFilter
    )

import Html exposing ( Html )
import Html.Attributes as HA
import String

import Listing
import Route exposing ( Route, clickRoute )
import Server
import Types exposing (..)

------------------------------------------------------------------------------------------------------------------------
-- Individual Files
------------------------------------------------------------------------------------------------------------------------

type AspectRatio
    = Square
    | Movie
    | Poster

thumb : AspectRatio -> FileId -> Html a
thumb aspect fid =
    let
        classes = HA.classList
            [ ( "thumb-container"           , True)
            , ( "thumb-container-square"    , aspect == Square )
            , ( "thumb-container-movie"     , aspect == Movie )
            , ( "thumb-container-poster"    , aspect == Poster )
            ]
    in
        Html.div [ classes ]
            [ Html.div
                [ HA.class "file-thumb"
                , HA.style [ ( "background-image", "url(" ++ Server.fileThumbUrl fid ++ ")" ) ]
                ] []
            ]

------------------------------------------------------------------------------------------------------------------------
-- Listings
------------------------------------------------------------------------------------------------------------------------

type alias ListModel =
    { files         : Listing.Model File
    , fileFilter    : WhichFiles
    , imageRouter   : Maybe (FileId -> Route)
    }

mkListModel : WhichFiles -> ListModel
mkListModel which =
    { files         = Listing.mkModel (Server.fetchFiles AllFiles)
    , fileFilter    = which
    , imageRouter   = Nothing
    }

withImageRouter : (FileId -> Route) -> ListModel -> ListModel
withImageRouter r m = { m | imageRouter = Just r }

setListItemCount : Int -> ListModel -> ListModel
setListItemCount cnt m = { m | files = Listing.withItemCount cnt m.files }

type ListAction
    = FileList (Listing.Action File)
    | RouteMsg Route.Route

setListFilter : WhichFiles -> ListModel -> (ListModel, Cmd ListAction)
setListFilter flt m =
    if (flt == m.fileFilter)
        then (m, Listing.refresh m.files |> Cmd.map FileList)
        else
            let
                (fs_, ffx)  = Listing.withFetchTask (Server.fetchFiles flt) m.files
            in
                ( { m | fileFilter = flt, files = fs_ }, Cmd.map FileList ffx )

viewList : ListModel -> Html ListAction
viewList m =
    let
        oneFile f =
            let
                fid = f.fileId
                clickWhat = if String.startsWith "image/" f.mimeType
                    then case m.imageRouter of
                        Nothing -> []
                        Just r  -> clickRoute (r fid)
                    else clickRoute (Route.Video fid)

            in
                Html.map RouteMsg <| Html.div [ HA.class "col-xs-3 col-md-2" ]
                    [ Html.a ( HA.class "thumbnail" :: clickWhat )
                        [ thumb Square fid ]
                    ]
    in
        List.map oneFile m.files.items |> Html.div [ HA.class "row" ]

listPagination : ListModel -> Html ListAction
listPagination m = Html.map FileList (Listing.pagination m.files)

updateListModel : ListAction -> ListModel -> (ListModel, Cmd ListAction)
updateListModel a m = case a of
    FileList a      -> Listing.update a m.files |> \(fs_, fx) -> ( { m | files = fs_ } , Cmd.map FileList fx )
    RouteMsg msg    -> (m, Route.go msg)
