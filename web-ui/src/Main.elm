
import Html exposing ( Html )
import Navigation

import Navbar
import Route
import User

type alias Model = User.WithModel
    { route : Route.Route }

type Msg
    = NoOp
    | UserMsg User.Msg
    | LocationChange Navigation.Location
    | NavAction Navbar.Action

init : Navigation.Location -> (Model, Cmd Msg)
init loc =
    let
        (um, uc) = User.init
    in
        ( { userModel = um, route = Route.parse loc }, Cmd.map UserMsg uc )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
    UserMsg um          -> User.update um m (Cmd.map UserMsg)
    NavAction na        -> case na of
        Navbar.GoTo r   -> (m, Route.go r )
        Navbar.LogOut   -> ( m, Cmd.none )
    NoOp                -> ( m, Cmd.none )
    LocationChange loc  -> ( { m | route = Route.parse loc }, Cmd.none )

view : Model -> Html Msg
view m =
    let
        mainpage = Html.div [] [ Html.map NavAction <| Navbar.view Nothing m.route ]
    in if User.loggedIn m.userModel
        then mainpage
        else User.view m.userModel |> Html.map UserMsg

main : Program Never Model Msg
main = Navigation.program LocationChange
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
