
module File (
    -- * Files
    thumb,

    -- * File Listings
    ListModel, mkListModel, ListAction(ImageSelected, VideoSelected), viewList, updateListModel, setListFilter
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Signal exposing ( Address )
import String
import Task

import Server
import Types exposing (..)
import Utils

------------------------------------------------------------------------------------------------------------------------
-- Individual Files
------------------------------------------------------------------------------------------------------------------------

thumb : FileId -> Html
thumb fid =
    Html.div [ HA.class "file-thumb-container" ]
        [ Html.div [ HA.class "file-thumb", HA.style [("background-image", "url(" ++ Server.fileThumbUrl fid ++ ")") ] ]
            [  ]
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
    = ImageSelected FileId
    | VideoSelected FileId
    | FilesLoaded (Result Http.Error (ApiList (FileId, File)))

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
                    then ImageSelected fid
                    else VideoSelected fid

            in
                Html.div [ HA.class "col-xs-3 col-md-2" ]
                    [ Html.a [ HA.class "thumbnail", Utils.onClick' aa clickWhat, HA.href <| Server.imageUrl fid ]
                        [ thumb fid ]
                    ]
    in
        List.map oneFile m.files |> Html.div [ HA.class "row" ]

updateListModel : ListAction -> ListModel -> ListModel
updateListModel a m = case a of
    ImageSelected _      -> m
    VideoSelected _      -> m
    FilesLoaded (Err x) -> Debug.log "failed loading files" x |> \_ -> m
    FilesLoaded (Ok l)  -> { m | files = l.items }
