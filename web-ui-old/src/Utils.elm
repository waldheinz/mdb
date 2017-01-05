
module Utils exposing (
    onClick_
    )

import Html exposing ( Html )
import Html.Events as HE
import Json.Decode as JD

------------------------------------------------------------------------------------------------------------------------
-- Html Events
------------------------------------------------------------------------------------------------------------------------

-- |
-- just like onClick, but with preventDefault set
onClick_ : a -> Html.Attribute a
onClick_ v =
    let
        do  = HE.defaultOptions
        do2 = { do | preventDefault = True }
    in
        HE.onWithOptions "click" do2 (JD.succeed v)
