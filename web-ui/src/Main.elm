
import Html exposing ( Html )
import Navigation

import Route
import User

type alias Model = User.WithModel
    { route : Route.Route }

type Msg
    = NoOp
    | UserMsg User.Msg
    | LocationChange Navigation.Location

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
    NoOp                -> ( m, Cmd.none )
    LocationChange loc  -> ( { m | route = Route.parse loc }, Cmd.none )

view : Model -> Html Msg
view m =
    if User.loggedIn m.userModel
        then Html.div [] [ Html.text "Hello World" ]
        else User.view m.userModel |> Html.map UserMsg

main : Program Never Model Msg
main = Navigation.program LocationChange
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
