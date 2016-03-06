
module Utils (
    onClick', editable
    ) where


import Html exposing ( Html )
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Signal exposing ( Address )

------------------------------------------------------------------------------------------------------------------------
-- Html Events
------------------------------------------------------------------------------------------------------------------------

-- |
-- just like onClick, but with preventDefault set
onClick' : Address a -> a -> Html.Attribute
onClick' aa v =
    let
        do  = HE.defaultOptions
        do' = { do | preventDefault = True }
    in
        HE.onWithOptions "click" do' JD.value (\_ -> Signal.message aa v)

editable : Address String -> String -> Html
editable aa t =
    let
        targetText = JD.at ["target", "innerText"] JD.string
    in
        Html.span [ HA.contenteditable True, HE.on "input" targetText (Signal.message aa) ]
            [ Html.text t ]
