
module Navbar exposing ( Action(..), view )

import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE

import Route exposing ( Route(..), clickRoute )

type Action
    = LogOut
    | GoTo Route

view : Maybe String -> Route -> Html Action
view muser r =
    let
        userName            = Maybe.withDefault "(not logged in)" muser
        pageLink pa pr pn   = Html.map GoTo <| Html.li
            [ HA.classList [ ( "active", pa ) ] ]
            [ Html.a (clickRoute pr) [ Html.text pn ] ]
        isAlbumList         = case r of
            AlbumList _ -> True
            _           -> False

    in
        Html.div [ HA.class "navbar navbar-default" ]
            [ Html.div [ HA.class "container-fluid" ]
                [ Html.div [ HA.class "navbar-header" ]
                    [ Html.button
                        [ HA.type_ "button"
                        , HA.class "navbar-toggle collapsed"
                        , HA.attribute "data-toggle" "collapse"
                        , HA.attribute "data-target" "#mdb-navbar"
                        ]
                        [ Html.span [ HA.class "icon-bar" ] []
                        , Html.span [ HA.class "icon-bar" ] []
                        , Html.span [ HA.class "icon-bar" ] []
                        ]
                    , Html.span [ HA.class "navbar-brand"] [ Html.text "MDB" ]
                    ]
                , Html.div [ HA.class "collapse navbar-collapse", HA.id "mdb-navbar" ]
                    [ Html.ul [ HA.class "nav navbar-nav" ]
                        [ pageLink (r == Home)      Home            "Home"
                        , pageLink (r == Series)    Series          "Series"
                        , pageLink isAlbumList      (AlbumList 0)   "Albums"
                        ]
                    , Html.ul [ HA.class "nav navbar-nav navbar-right" ]
                        [ Html.li [ HA.class "dropdown" ]
                            [ Html.a [ HA.href "#", HA.class "dropdown-toggle", HA.attribute "data-toggle" "dropdown" ]
                                [ Html.text userName, Html.span [ HA.class "caret"] [] ]
                            , Html.ul [ HA.class "dropdown-menu" ]
                                [ Html.li [ ] [ Html.a [ HA.href "#", HE.onClick LogOut ] [ Html.text "log out"] ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
