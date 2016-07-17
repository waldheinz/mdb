
module Utils exposing (
    onClick'
    )

import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD

------------------------------------------------------------------------------------------------------------------------
-- Html Events
------------------------------------------------------------------------------------------------------------------------

-- |
-- just like onClick, but with preventDefault set
onClick' : a -> Html.Attribute a
onClick' v =
    let
        do  = HE.defaultOptions
        do' = { do | preventDefault = True }
    in
        HE.onWithOptions "click" do' (JD.succeed v)
