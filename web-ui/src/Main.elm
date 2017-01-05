
import Html exposing ( Html )

import User

type alias Model = User.WithModel {}

type Msg
    = NoOp
    | UserMsg User.Msg

init : (Model, Cmd Msg)
init =
    let
        (um, uc) = User.init
    in
        ( { userModel = um }, Cmd.map UserMsg uc )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
    UserMsg um  -> User.update um m (Cmd.map UserMsg)
    NoOp        -> ( m, Cmd.none )

view : Model -> Html Msg
view m =
    if User.loggedIn m.userModel
        then Html.div [] [ Html.text "Hello World" ]
        else User.view m.userModel |> Html.map UserMsg

main : Program Never Model Msg
main = Html.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
