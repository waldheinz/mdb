
module Utils (
    onClick'
    ) where


import Html
import Html.Events as HE
import Json.Decode as JD
import Json.Encode as JE
import Signal

------------------------------------------------------------------------------------------------------------------------
-- Html Events
------------------------------------------------------------------------------------------------------------------------

-- |
-- just like onClick, but with preventDefault set
onClick' : Signal.Address a -> a -> Html.Attribute
onClick' aa v =
    let
        do  = HE.defaultOptions
        do' = { do | preventDefault = True }
    in
        HE.onWithOptions "click" do' JD.value (\_ -> Signal.message aa v)
