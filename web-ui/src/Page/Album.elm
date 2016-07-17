
module Page.Album exposing (
    Model, initialModel, view,
    Action, update, onMount
    )

import Html exposing ( Html )
import Html.App
import Html.Attributes as HA
import Http
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

noOp : Cmd () -> Cmd Action
noOp = Cmd.map (\() -> NoOp)

onMount : AlbumId -> Maybe FileId -> Model-> (Model, Cmd Action)
onMount aid mfid m =
    let
        (fl', ffx)  = File.setListFilter (AlbumFiles aid) m.files
        (pl', plfx) = Person.setListFilter (AlbumPersons aid) m.persons
        a'          = if aid == m.albumId then m.album else Nothing
        fetchAlbum  = Server.fetchAlbumInfo aid |> Task.perform (Err >> GotAlbumInfo) (Ok >> GotAlbumInfo)
    in
        (   { m
            | albumId   = aid
            , album     = a'
            , files     = withImageRouter (\fid -> Route.Album aid (Just fid)) fl'
            , persons   = pl'
            , bigItem   = mfid
            }
        ,   Cmd.batch
                [ Cmd.map FileListAction ffx
                , Cmd.map PersonListAction plfx
                , fetchAlbum
                ]
        )

view : Model -> Html Action
view m =
    let
        albumName = case m.album of
            Nothing -> "Album " ++ toString m.albumId
            Just a  -> a.name

        fileList =
            Html.div [ HA.class "container" ] <| List.filterMap identity
                [ Maybe.map bigFile m.bigItem
                , Just <| Html.h1 [ HA.class "page-lead" ] [ Html.text albumName ]
                , Just <| Html.div [ HA.class "text-center" ]
                    [ Html.App.map FileListAction (File.listPagination m.files) ]
                , Just <| Html.App.map FileListAction (File.viewList m.files)
                , if Person.listEmpty m.persons
                    then Nothing
                    else Just <| Html.div []
                        [ Html.h2 [] [ Html.text "Persons in this Album" ]
                        , Html.App.map PersonListAction (Person.viewList m.persons)
                        ]
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

update : Action -> Model -> (Model, Cmd Action)
update a m = case a of
    NoOp                    -> (m, Cmd.none)
    FileListAction la       -> File.updateListModel la m.files
        |> \(fs', ffx) -> ({ m | files = fs' }, Cmd.map FileListAction ffx)
    PersonListAction pla    -> ({ m | persons = Person.updateListModel pla m.persons }, Cmd.none)
    GotAlbumInfo (Err er)   -> Debug.log "fetching album info failed" er |> \_ -> ( m, Cmd.none )
    GotAlbumInfo (Ok a)     -> ( { m | album = Just a, files = File.setListItemCount a.fileCount m.files }, Cmd.none )
