
module Page.Person exposing (
    Model, Action, view, update, onMount, initialModel
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Http
import Task

import Album exposing (ListAction(..))
import File
import Page.Album
import Person
import Server
import Types exposing ( .. )
import Utils

type alias Model =
    { personId      : PersonId
    , person        : Maybe Person
    , albums        : Album.ListModel
    , albumPage     : Page.Album.Model
    , randomFiles   : File.ListModel
    }

initialModel : Model
initialModel =
    { personId      = 0
    , person        = Nothing
    , albums        = Album.initialListModel
    , albumPage     = Page.Album.initialModel
    , randomFiles   = File.mkListModel AllFiles
    }

type Action
    = NoOp
    | AlbumListAction Album.ListAction
    | FileListAction File.ListAction
    | PersonLoaded (Result Http.Error Person)
    | ChangeName String

noOp : Effects () -> Effects Action
noOp = Effects.map (\() -> NoOp)

onMount : PersonId -> Model -> (Model, Effects Action)
onMount pid m =
    let
        (rf', rffx) = File.setListFilter (PersonNoAlbum pid) m.randomFiles
        (as', fa)   = Album.withListFilter (PersonAlbums pid) m.albums
        p'          = if pid == m.personId
            then m.person
            else Nothing
    in
        ( { m | personId = pid, randomFiles = rf', person = p', albums = as' }
        , Effects.batch
            [ Effects.map AlbumListAction fa
            , Effects.map FileListAction rffx
            , Server.fetchPerson pid |> Task.toResult |> Task.map PersonLoaded |> Effects.task
            ]
        )

view : Signal.Address Action -> Model -> Html
view aa m =
    let
        pname = case m.person of
            Nothing -> "Person #" ++ toString m.personId
            Just p  -> p.name
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-header" ] [ Utils.editable (Signal.forwardTo aa ChangeName) pname ]
            , Html.h2 [] [ Html.text "Albums" ]
            , Html.div [ HA.class "text-center" ]
                [ Album.listPagination (Signal.forwardTo aa AlbumListAction) m.albums ]
            , Album.viewList (Signal.forwardTo aa AlbumListAction) m.albums
            , Html.h2 [] [ Html.text "Random Files" ]
            , File.viewList (Signal.forwardTo aa FileListAction) m.randomFiles
            ]

update : Action -> Model -> (Model, Effects Action)
update a m = case a of
    NoOp                                -> (m, Effects.none)
    ChangeName n                        -> case m.person of
        Nothing -> (m, Effects.none)
        Just p ->
            let
                (p', pfx) = Person.updatePerson (\p -> { p | name = n }) m.personId p
            in
                ( { m | person = Just p' }, noOp pfx)

    PersonLoaded (Err err)              -> Debug.log "loading person failed" err |> \_ -> (m, Effects.none)
    PersonLoaded (Ok p)                 -> ( { m | person = Just p}, Effects.none )
    FileListAction fla                  -> File.updateListModel fla m.randomFiles
        |> \ (rfs', rffx) -> ( { m | randomFiles = rfs' }, Effects.map FileListAction rffx)
    AlbumListAction aa                  ->
        let
            (as', afx) = Album.updateList aa m.albums
        in
            ( { m | albums = as' }, Effects.map AlbumListAction afx )
