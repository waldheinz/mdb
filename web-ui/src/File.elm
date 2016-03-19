
module File (
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
                Html.div [ HA.class "file-thumb" ]
                    [ Html.a [ HA.class "", Utils.onClick' aa clickWhat, HA.href <| Server.imageUrl fid ]
                        [ Html.img [ HA.src (Server.fileThumbUrl fid)  ] [] ]
                    ]
    in
        List.map oneFile m.files |> Html.div [ HA.class "thumb-container" ]

updateListModel : ListAction -> ListModel -> ListModel
updateListModel a m = case a of
    ImageSelected _      -> m
    VideoSelected _      -> m
    FilesLoaded (Err x) -> Debug.log "failed loading files" x |> \_ -> m
    FilesLoaded (Ok l)  -> { m | files = l.items }
