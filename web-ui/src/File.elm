
module File (
    -- * Files
    AspectRatio(..), thumb,

    -- * File Listings
    ListModel, mkListModel, ListAction, viewList, updateListModel, setListFilter
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Signal exposing ( Address )
import String
import Task

import Route exposing ( clickRoute )
import Server
import Types exposing (..)

------------------------------------------------------------------------------------------------------------------------
-- Individual Files
------------------------------------------------------------------------------------------------------------------------

type AspectRatio
    = Square
    | Movie
    | Poster

thumb : AspectRatio -> FileId -> Html
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
    { files         : List (FileId, File)
    , fileFilter    : WhichFiles
    }

mkListModel : WhichFiles -> ListModel
mkListModel which =
    { files         = []
    , fileFilter    = which
    }

type ListAction
    = FilesLoaded (Result Http.Error (ApiList (FileId, File)))

setListFilter : WhichFiles -> ListModel -> (ListModel, Effects ListAction)
setListFilter which m =
    let
        fs' = if which == m.fileFilter then m.files else []
    in
        ( { m | fileFilter = which, files = fs' }
        , Server.fetchFiles which |> Task.toResult |> Task.map FilesLoaded |> Effects.task
        )

viewList : Address ListAction -> ListModel -> Html
viewList aa m =
    let
        oneFile (fid, f) =
            let
                clickWhat = if String.startsWith "image/" f.mimeType
                    then [ HA.href <| Server.imageUrl fid ]
                    else clickRoute (Route.Video fid)

            in
                Html.div [ HA.class "col-xs-3 col-md-2" ]
                    [ Html.a ( HA.class "thumbnail" :: clickWhat )
                        [ thumb Square fid ]
                    ]
    in
        List.map oneFile m.files |> Html.div [ HA.class "row" ]

updateListModel : ListAction -> ListModel -> ListModel
updateListModel a m = case a of
    FilesLoaded (Err x) -> Debug.log "failed loading files" x |> \_ -> m
    FilesLoaded (Ok l)  -> { m | files = l.items }
