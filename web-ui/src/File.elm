
module File (
    -- * File Listings
    ListModel, mkListModel, ListAction, viewList, updateListModel, setListFilter
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD exposing ( (:=) )
import Signal exposing ( Address )
import Task

import Server
import Types exposing (..)

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
    = FileSelected FileId
    | FilesLoaded (Result Http.Error (Server.ApiList (FileId, File)))

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
            Html.div [ HA.class "file-thumb" ]
                [ Html.a [ HA.class "thumbnail", HE.onClick aa (FileSelected fid), HA.href "#" ]
                    [ Html.img [ HA.src <| Server.fileThumbUrl fid ] [] ]
                ]
    in
        List.map oneFile m.files |> Html.div [ HA.class "thumb-container" ]

updateListModel : ListAction -> ListModel -> ListModel
updateListModel a m = case a of
    FileSelected _      -> m
    FilesLoaded (Err x) -> Debug.log "failed loading files" x |> \_ -> m
    FilesLoaded (Ok l)  -> { m | files = l.items }
