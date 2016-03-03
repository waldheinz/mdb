
module Page.Album (
    Model, initialModel, view,
    Action, update, onMount
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Signal exposing ( Address )

import File
import Types exposing (..)

type alias Model =
    { albumId   : AlbumId
    , files     : File.ListModel
    }

initialModel : Model
initialModel =
    { albumId   = 0
    , files     = File.mkListModel AllFiles
    }

type Action
    = NoOp
    | FileListAction File.ListAction

onMount : AlbumId -> Model-> (Model, Effects Action)
onMount aid m =
    let
        (fl', ffx) = File.setListFilter (AlbumFiles aid) m.files
    in
        ({ m | albumId = aid, files = fl' }, Effects.map FileListAction ffx)

view : Address Action -> Model -> Html
view aa m =
    Html.div []
        [ Html.text "album"
        , File.viewList (Signal.forwardTo aa FileListAction) m.files
        ]

update : Action -> Model -> Model
update a m = case a of
    NoOp    -> m
    FileListAction la   -> { m | files = File.updateListModel la m.files }
