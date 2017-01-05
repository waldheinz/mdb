
module User exposing (
    WithModel, Msg, init, update, loggedIn, view
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as JD
import Json.Encode as JE

type Msg
    = NoOp
    | CheckedUser (Result Http.Error String)
    | LoginResponse (Result Http.Error String)
    | DoLogin
    | SetLoginUser String
    | SetLoginPass String

type alias LoginModel =
    { userName : String
    , password : String
    }

type Model
    = UnknownState
    | LogIn LoginModel
    | LoggedIn String

type alias WithModel a = { a | userModel : Model }

checkUser : Cmd Msg
checkUser = Http.send CheckedUser (Http.getString "/api/user/self" )

doLogin : LoginModel -> Cmd Msg
doLogin l =
    let
        json    = JE.object [ ( "user", JE.string l.userName ), ("pass", JE.string l.password ) ]
        decoder = JD.at ["userName"] JD.string
        req     = Http.request
            { method    = "POST"
            , headers   = [ Http.header "Content-Type" "application/json" ]
            , url       = "/api/user/self/login"
            , body      = Http.jsonBody json
            , expect    = Http.expectJson decoder
            , timeout   = Nothing
            , withCredentials = False
            }
    in
        Http.send LoginResponse req

init : (Model, Cmd Msg)
init = ( UnknownState, checkUser )

update : Msg -> WithModel a -> (Cmd Msg -> Cmd m )-> (WithModel a, Cmd m)
update msg wm fc = case msg of
    NoOp -> ( wm, Cmd.none )
    CheckedUser (Ok u)  -> ( { wm | userModel = LoggedIn u }, Cmd.none )
    CheckedUser _       -> ( { wm | userModel = LogIn { userName = "", password = "" } }, Cmd.none )
    DoLogin             ->
        case wm.userModel of
            LogIn lm    -> ( wm, fc <| doLogin lm )
            _           -> ( wm, Cmd.none )
    SetLoginUser u      ->
        case wm.userModel of
            LogIn lm    -> ( { wm | userModel = LogIn { lm | userName = u } }, Cmd.none )
            _           -> ( wm, Cmd.none )
    SetLoginPass p ->
        case wm.userModel of
            LogIn lm    -> ( { wm | userModel = LogIn { lm | password = p } }, Cmd.none )
            _           -> ( wm, Cmd.none )
    LoginResponse (Ok u)    -> ( { wm | userModel = LoggedIn u }, Cmd.none )
    LoginResponse (Err e)   -> ( wm, Cmd.none )

loggedIn : Model -> Bool
loggedIn m = case m of
    _ -> False

loginView : LoginModel -> Html Msg
loginView m =
    Html.div [ HA.class "container" ]
        [ Html.div [ HA.class "jumbotron" ]
            [ Html.h1 [] [ Html.text "Login Required" ]
            , Html.form [ HA.class "clearfix" ]
                [ Html.div [ HA.class "form-group" ]
                    [ Html.label [] [ Html.text "Username" ]
                    , Html.input
                        [ HA.class "form-control", HA.id "mdbUser" , HE.onInput SetLoginUser ] []
                    ]
                , Html.div [ HA.class "form-group" ]
                    [ Html.label [] [ Html.text "Password" ]
                    , Html.input
                        [ HA.type_ "password", HA.class "form-control", HA.id "mdbPassword" , HE.onInput SetLoginPass ]
                        []
                    ]
                , Html.button [ HA.type_ "button", HA.class "btn btn-primary pull-right", HE.onClick DoLogin ]
                    [ Html.text "login" ]
                ]
            ]
        ]

view : Model -> Html Msg
view m = case m of
    UnknownState    -> Html.div [] [ Html.text "checking login state..." ]
    LogIn lm        -> loginView lm
    LoggedIn u      -> Html.div [] []
