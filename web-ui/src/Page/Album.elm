
module Page.Album (
    Model, initialModel, view,
    Action, update, onMount
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Signal exposing ( Address )

import File
import Person
import Route
import Types exposing (..)

type alias Model =
    { albumId   : AlbumId
    , album     : Maybe Album
    , files     : File.ListModel
    , persons   : Person.ListModel
    }

initialModel : Model
initialModel =
    { albumId   = 0
    , album     = Nothing
    , files     = File.mkListModel AllFiles
    , persons   = Person.initialListModel
    }

type Action
    = NoOp
    | FileListAction File.ListAction
    | PersonListAction Person.ListAction

noOp : Effects () -> Effects Action
noOp = Effects.map (\() -> NoOp)

onMount : AlbumId -> Model-> (Model, Effects Action)
onMount aid m =
    let
        (fl', ffx)  = File.setListFilter (AlbumFiles aid) m.files
        (pl', plfx) = Person.setListFilter (AlbumPersons aid) m.persons
    in
        ( { m | albumId = aid, files = fl', persons = pl' }
        , Effects.batch [ Effects.map FileListAction ffx, Effects.map PersonListAction plfx ]
        )

view : Address Action -> Model -> Html
view aa m =
    Html.div [ HA.class "container" ]
        [ Html.h1 [ HA.class "page-lead" ] [ Html.text <| "Album " ++ toString m.albumId ]
        , Person.viewList (Signal.forwardTo aa PersonListAction) m.persons
        , File.viewList (Signal.forwardTo aa FileListAction) m.files
        ]

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    NoOp                                -> (m, Effects.none)
    FileListAction (File.VideoSelected fid) ->
        (m, Route.goRoute (Route.Video fid) |> noOp)
    FileListAction la                   -> ({ m | files = File.updateListModel la m.files }, Effects.none)
    PersonListAction pla                -> ({ m | persons = Person.updateListModel pla m.persons }, Effects.none)
