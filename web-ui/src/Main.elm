
import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE

type alias Model = { }

type Msg = NoOp

init : (Model, Cmd Msg)
init = ( { }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = ( m, Cmd.none )

view : Model -> Html Msg
view _ = Html.div [] [ Html.text "Hello World" ]

main : Program Never Model Msg
main = Html.program
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
