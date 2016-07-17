
module Page.Person exposing (
    Model, Action, view, update, onMount, initialModel
    )

import Html exposing ( Html )
import Html.App
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

noOp : Cmd () -> Cmd Action
noOp = Cmd.map (\() -> NoOp)

onMount : PersonId -> Model -> (Model, Cmd Action)
onMount pid m =
    let
        (rf', rffx) = File.setListFilter (PersonNoAlbum pid) m.randomFiles
        (as', fa)   = Album.withListFilter (PersonAlbums pid) m.albums
        p'          = if pid == m.personId
            then m.person
            else Nothing
    in
        ( { m | personId = pid, randomFiles = rf', person = p', albums = as' }
        , Cmd.batch
            [ Cmd.map AlbumListAction fa
            , Cmd.map FileListAction rffx
            , Server.fetchPerson pid |> Task.perform (Err >> PersonLoaded) (Ok >> PersonLoaded)
            ]
        )

view : Model -> Html Action
view m =
    let
        pname = case m.person of
            Nothing -> "Person #" ++ toString m.personId
            Just p  -> p.name
    in
        Html.div [ HA.class "container" ]
            [ Html.h1 [ HA.class "page-header" ] [ Html.text pname ]
            , Html.h2 [] [ Html.text "Albums" ]
            , Html.div [ HA.class "text-center" ]
                [ Html.App.map AlbumListAction (Album.listPagination m.albums) ]
            , Html.App.map AlbumListAction (Album.viewList m.albums)
            , Html.h2 [] [ Html.text "Random Files" ]
            , Html.App.map FileListAction (File.viewList m.randomFiles)
            ]

update : Action -> Model -> (Model, Cmd Action)
update a m = case a of
    NoOp                                -> (m, Cmd.none)
    ChangeName n                        -> case m.person of
        Nothing -> (m, Cmd.none)
        Just p ->
            let
                (p', pfx) = Person.updatePerson (\p -> { p | name = n }) m.personId p
            in
                ( { m | person = Just p' }, noOp pfx)

    PersonLoaded (Err err)              -> Debug.log "loading person failed" err |> \_ -> (m, Cmd.none)
    PersonLoaded (Ok p)                 -> ( { m | person = Just p}, Cmd.none )
    FileListAction fla                  -> File.updateListModel fla m.randomFiles
        |> \ (rfs', rffx) -> ( { m | randomFiles = rfs' }, Cmd.map FileListAction rffx)
    AlbumListAction aa                  ->
        let
            (as', afx) = Album.updateList aa m.albums
        in
            ( { m | albums = as' }, Cmd.map AlbumListAction afx )
