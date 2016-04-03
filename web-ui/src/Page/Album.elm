
module Page.Album (
    Model, initialModel, view,
    Action, update, onMount
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Signal exposing ( Address )
import Task

import File exposing ( withImageRouter )
import Person
import Route
import Server
import Types exposing (..)

type alias Model =
    { albumId   : AlbumId
    , album     : Maybe Album
    , files     : File.ListModel
    , persons   : Person.ListModel
    , bigItem   : Maybe FileId      -- ^ currently shown image/video from this album, otherwise list
    }

initialModel : Model
initialModel =
    { albumId   = 0
    , album     = Nothing
    , files     = File.mkListModel AllFiles
    , persons   = Person.initialListModel
    , bigItem   = Nothing
    }

type Action
    = NoOp
    | FileListAction File.ListAction
    | PersonListAction Person.ListAction
    | GotAlbumInfo (Result Http.Error Album)

noOp : Effects () -> Effects Action
noOp = Effects.map (\() -> NoOp)

onMount : AlbumId -> Maybe FileId -> Model-> (Model, Effects Action)
onMount aid mfid m =
    let
        (fl', ffx)  = File.setListFilter (AlbumFiles aid) m.files
        (pl', plfx) = Person.setListFilter (AlbumPersons aid) m.persons
        a'          = if aid == m.albumId then m.album else Nothing
        fetchAlbum  = Server.fetchAlbumInfo aid |> Task.toResult |> Task.map GotAlbumInfo |> Effects.task
    in
        (   { m
            | albumId   = aid
            , album     = a'
            , files     = withImageRouter (\fid -> Route.Album aid (Just fid)) fl'
            , persons   = pl'
            , bigItem   = mfid
            }
        ,   Effects.batch
                [ Effects.map FileListAction ffx
                , Effects.map PersonListAction plfx
                , fetchAlbum
                ]
        )

view : Address Action -> Model -> Html
view aa m =
    let
        albumName = case m.album of
            Nothing -> "Album " ++ toString m.albumId
            Just a  -> a.name

        fileList =
            Html.div [ HA.class "container" ] <| List.filterMap identity
                [ Maybe.map bigFile m.bigItem
                , Just <| Html.h1 [ HA.class "page-lead" ] [ Html.text albumName ]
                , Just <| File.viewList (Signal.forwardTo aa FileListAction) m.files
                , Just <| Html.h2 [] [ Html.text "Persons in this Album" ]
                , Just <| Person.viewList (Signal.forwardTo aa PersonListAction) m.persons
                ]

        bigFile fid =
            Html.div [ HA.class "big-image-container", HA.style [ ("background-image", "url(" ++ Server.fileThumbUrl fid ++ ")")] ]
                [ Html.img
                    [ HA.src <| Server.imageUrl fid
                    , HA.class "big-image"

                    ] []
                ]
    in
        fileList

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    NoOp                                -> (m, Effects.none)
    FileListAction la                   -> ({ m | files = File.updateListModel la m.files }, Effects.none)
    PersonListAction pla    -> ({ m | persons = Person.updateListModel pla m.persons }, Effects.none)
    GotAlbumInfo (Err er)   -> Debug.log "fetching album info failed" er |> \_ -> ( m, Effects.none )
    GotAlbumInfo (Ok a)     -> ( { m | album = Just a }, Effects.none )
